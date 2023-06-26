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
alias k=kubectl
alias kcgc='kubectl config get-contexts' # display your current context information which is really useful when dealing with multiple clusters
alias kcuc='kubectl config use-context' # helps in quickly switching the context
alias kr='kubectl run' # help you quickly start a new pod
alias kaf='kubectl apply -f' # apply the configuration from a YAML file
alias kex='kubectl exec -i -t'
alias kg='kubectl get' # list a ressource
alias kgn2='alias kgn2='kubectl get nodes -o custom-columns=Hostname:".metadata.labels.kubernetes\.io/hostname",Name:".metadata.name",Role:".metadata.labels.node\.kubernetes\.io/role",Kubelet:.status.nodeInfo.kubeletVersion,Status:.status.conditions[-1].reason,Date:".metadata.creationTimestamp",InstType:".metadata.labels.beta\.kubernetes\.io/instance-type,Cpu Cores:.status.capacity.cpu,Ram:.status.capacity.memory,Taints:.spec.taints"''
alias kgp='kubectl get pods' # list all pods in the current namespace
alias kgs='kubectl get svc' # list all services in the current namespace
alias kgsec='kubectl get secret'
alias kd='kubectl describe' # get detailed information about a resource
alias kdp='kubectl describe pod'
alias kds='kubectl describe service'
alias kdsec='kubectl describe secret'
alias krm='kubectl delete' # delete a specified resource
alias kga='kubectl get all' # list all resources in the current namespace

