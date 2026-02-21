package main

import (
	"context"
	"encoding/csv"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"os"
	"strings"

	cloudasset "google.golang.org/api/cloudasset/v1"
	policyanalyzer "google.golang.org/api/policyanalyzer/v1"
)

// Instructions:
// 1. Ensure you have your modules initialized: `go mod init audit`
// 2. Fetch dependencies:
//    `go get google.golang.org/api/cloudasset/v1`
//    `go get google.golang.org/api/policyanalyzer/v1`
// 3. Authenticate with ADC: `gcloud auth application-default login`
// 4. Run the script: `go run google_cloud_security_audit.go -org=1234567890`

func main() {
	orgID := flag.String("org", "", "Organization ID to audit (e.g., 1234567890)")
	out := flag.String("out", "sa_keys_audit.csv", "Output CSV file path")
	flag.Parse()

	if *orgID == "" {
		log.Fatal("Organization ID is required. Use -org flag.")
	}

	ctx := context.Background()

	// Initialize the Cloud Asset Inventory v1 service
	assetService, err := cloudasset.NewService(ctx)
	if err != nil {
		log.Fatalf("Failed to create Asset service: %v", err)
	}

	// Initialize the Policy Analyzer v1 service
	policyAnalyzerService, err := policyanalyzer.NewService(ctx)
	if err != nil {
		log.Fatalf("Failed to create Policy Analyzer service: %v", err)
	}

	// Create the output CSV file
	csvFile, err := os.Create(*out)
	if err != nil {
		log.Fatalf("Failed to create CSV file %s: %v", *out, err)
	}
	defer csvFile.Close()

	writer := csv.NewWriter(csvFile)
	defer writer.Flush()

	// Write the CSV header
	header := []string{"Project ID", "Service Account", "Key Name", "Asset Name", "Creation Date", "Last Usage Timestamp"}
	if err := writer.Write(header); err != nil {
		log.Fatalf("Failed to write CSV header: %v", err)
	}

	log.Printf("Phase 1: Querying Cloud Asset Inventory for all Service Account Keys in org %s...", *orgID)

	scope := fmt.Sprintf("organizations/%s", *orgID)
	req := assetService.V1.SearchAllResources(scope).
		AssetTypes("iam.googleapis.com/ServiceAccountKey")

	var allKeys []*cloudasset.ResourceSearchResult
	err = req.Pages(ctx, func(page *cloudasset.SearchAllResourcesResponse) error {
		allKeys = append(allKeys, page.Results...)
		return nil
	})

	if err != nil {
		log.Fatalf("Failed to search all resources in CAI: %v", err)
	}

	log.Printf("Found %d service account keys via CAI.", len(allKeys))

	log.Printf("Phase 2: Querying Policy Analyzer for key usage activity...")

	// Policy Analyzer can query at the organization level to get all key usage activities
	// The activity type is serviceAccountKeyLastAuthentication
	activityParent := fmt.Sprintf("organizations/%s/locations/global/activityTypes/serviceAccountKeyLastAuthentication", *orgID)

	// Create a map to quickly look up last usage by full key name
	keyUsageMap := make(map[string]string)

	activityReq := policyAnalyzerService.Organizations.Locations.ActivityTypes.Activities.Query(activityParent)

	// Handle pagination manually for Policy Analyzer Query
	pageToken := ""
	for {
		if pageToken != "" {
			activityReq.PageToken(pageToken)
		}

		resp, err := activityReq.Do()
		if err != nil {
			log.Printf("Warning: Failed to query Policy Analyzer activities at org level: %v. "+
				"Note that you need the 'roles/policyanalyzer.activityViewer' or similar permissions.", err)
			break
		}

		for _, activity := range resp.Activities {
			// Extract the full key name from the activity.
			// The activity's fullResourceName format is typically identical to the CAI asset name.

			// We unmarshal the raw JSON bytes to find the fullResourceName
			var actMap map[string]interface{}
			if err := json.Unmarshal(activity.Activity, &actMap); err == nil {
				if fullResName, ok := actMap["fullResourceName"].(string); ok {
					// Also grab the timestamp
					var timestamp string
					if authInfo, ok := actMap["authenticationInfo"].(map[string]interface{}); ok {
						if ts, ok := authInfo["lastAuthenticationTime"].(string); ok {
							timestamp = ts
						}
					}

					if timestamp != "" {
						keyUsageMap[fullResName] = timestamp
					}
				}
			}
		}

		if resp.NextPageToken == "" {
			break
		}
		pageToken = resp.NextPageToken
	}

	log.Printf("Phase 3: Merging data and writing to CSV...")

	count := 0
	for _, keyAsset := range allKeys {
		// keyAsset.Project is in format "projects/ID"
		projectID := strings.TrimPrefix(keyAsset.Project, "projects/")

		// Extract SA Email and Key Name from the CAI asset name
		// Example Name: //iam.googleapis.com/projects/my-project/serviceAccounts/my-sa@my-project.iam.gserviceaccount.com/keys/1234abcd
		assetName := keyAsset.Name
		saEmail := "Unknown"
		keyName := "Unknown"

		parts := strings.Split(assetName, "/")
		for i, part := range parts {
			if part == "serviceAccounts" && i+1 < len(parts) {
				saEmail = parts[i+1]
			}
			if part == "keys" && i+1 < len(parts) {
				keyName = parts[i+1]
			}
		}

		creationDate := keyAsset.CreateTime

		// Look up the last usage timestamp from the Policy Analyzer results
		lastUsage := keyUsageMap[assetName]
		if lastUsage == "" {
			lastUsage = "No usage found (or no data)"
		}

		row := []string{
			projectID,
			saEmail,
			keyName,
			assetName,
			creationDate,
			lastUsage,
		}

		if err := writer.Write(row); err != nil {
			log.Printf("  -> Failed to write row to CSV: %v", err)
		} else {
			count++
		}
	}

	log.Printf("Audit completed successfully. %d rows saved to %s", count, *out)
}
