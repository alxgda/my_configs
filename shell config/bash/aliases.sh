alias meteo="curl wttr.in/Geneva" # https://github.com/chubin/wttr.in
alias ebhrc="vim ~/.bashrc"
alias ezhrc="vim ~/.zshrc"

# RESEAU
alias publicip="curl ipv4.icanhazip.com"
alias localip="ipconfig getifaddr en0" #macOS

# BUREAUTIQUE (find, grep...)
alias showhidden="defaults write com.apple.Finder AppleShowAllFiles true && killall Finder" # macOS
alias hidehidden="defaults write com.apple.Finder AppleShowAllFiles false && killall Finder" # macOS

# TRAITEMENT DE CSV

# cURL

# OPENSSL

# OPENSSH

# DOCKER

# KUBERNETES
alias k=kubectl # shows the version of kubectl and the Kubernetes cluster that it's currently communicating with
alias kv='kubectl version' 
alias kcogc='kubectl config get-contexts' # display list of contexts
alias kcocc='kubectl config current-context' # display the current-context
alias kcouc='kubectl config use-context' # set the default context to the current cluster
alias kcosc='kubectl config set-context --current' # Edit the current context properties
alias kaf='kubectl apply -f' # apply the configuration from a YAML file
alias kex='kubectl exec -it'
alias kg='kubectl get' # list a ressource
alias kd='kubectl describe' # get detailed information about a resource
alias krm='kubectl delete' # delete a specified resource
alias kga='kubectl get all' # list all resources in the current namespace
alias kgn2='alias kgn2='kubectl get nodes -o custom-columns=Hostname:".metadata.labels.kubernetes\.io/hostname",Name:".metadata.name",Role:".metadata.labels.node\.kubernetes\.io/role",Kubelet:.status.nodeInfo.kubeletVersion,Status:.status.conditions[-1].reason,Date:".metadata.creationTimestamp",InstType:".metadata.labels.beta\.kubernetes\.io/instance-type,Cpu Cores:.status.capacity.cpu,Ram:.status.capacity.memory,Taints:.spec.taints"''
alias kr='kubectl run' # help you quickly start a new pod
alias kcd='kubectl create deployment'
alias kgd='kubectl get deployments' 
alias kdd='kubectl describe deployment'
alias krmd='kubectl delete deployment'
alias kcp='kubectl create pod'
alias kgp='kubectl get pods' # list all pods in the current namespace
alias kdp='kubectl describe pod'
alias krmp='kubectl delete pod'
alias kgs='kubectl get service' # list all services in the current namespace
alias kds='kubectl describe service'
alias krms='kubectl delete service'



