# GCP Service Account Key Audit Script

This automation script allows Google Cloud Administrators to efficiently audit the Service Account Keys used across their entire GCP Organization. It discovers all active Service Account keys and correlates them with exact last usage timestamps. The output is a clear CSV report.

## Architecture & Design Decisions

### 1. Cloud Asset Inventory (CAI) for Discovery
The script utilizes the Cloud Asset Inventory (CAI) API to discover Service Account keys instead of iterating through every GCP project and service account manually. 
**Why?** Iterating through thousands of projects and service accounts natively using the Resource Manager and IAM APIs is extremely slow and requires thousands of API calls. The CAI `SearchAllResources` API provides a single, declarative, and highly-optimized mechanism to query for all `iam.googleapis.com/ServiceAccountKey` resources across the organization instantly.

### 2. Policy Analyzer (Activity API) for Usage Timestamps
The Cloud Asset Inventory API only returns resource metadata; it does not track dynamic usage or metrics. To identify exactly when a service account key was last used to authenticate against a Google service, the script dynamically queries the Google Cloud Policy Analyzer (specifically the `serviceAccountKeyLastAuthentication` activity type).
**Why?** This is the only scalable way to reliably pull exact usage metrics for keys org-wide without having to deploy complex Log Sinks to parse heavy Cloud Audit Logs dynamically.

### 3. API Client Library Choices (Go SDK)
This script explicitly imports the auto-generated REST/JSON Google APIs Client Libraries for Go (`google.golang.org/api/cloudasset/v1` and `google.golang.org/api/policyanalyzer/v1`), rather than the modern, idiomatic Google Cloud Client Libraries (`cloud.google.com/go/...`).
**Why?**
* **Lack of Modern Client for Policy Analyzer**: Currently, the Activity API (needed for the `serviceAccountKeyLastAuthentication` metric) does not have a comprehensive, stable gRPC client in the `cloud.google.com/go/...` namespace. 
* **Consistency**: To maintain a consistent codebase, authentication flow, and error-handling pattern within the same script, the Cloud Asset Inventory API also utilizes its corresponding auto-generated REST wrapper (`google.golang.org/api/cloudasset/v1`).

## Usage Prerequisites

1. Initialize your Go modules and download dependencies:
   ```bash
   go mod init audit
   go get google.golang.org/api/cloudasset/v1
   go get google.golang.org/api/policyanalyzer/v1
   go mod tidy
   ```
2. Authenticate the environment using Application Default Credentials (ADC):
   ```bash
   gcloud auth application-default login
   ```
3. Ensure the authenticated principal has the following organization-level IAM roles:
   * `roles/cloudasset.viewer`
   * `roles/policyanalyzer.activityViewer`

## Running the Script

You must provide your Google Cloud Organization ID (so it can discover all child projects).
```bash
go run google_cloud_security_audit.go -org=YOUR_ORG_ID
```
*(You can optionally use the `-out=my_custom_file.csv` flag to specify a different output path).*
