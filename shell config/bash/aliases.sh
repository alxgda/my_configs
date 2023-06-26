alias meteo="curl wttr.in/Geneva" # https://github.com/chubin/wttr.in
alias ebhrc="vim ~/.bashrc"
alias ezhrc="vim ~/.zshrc"
alias publicip="curl ipv4.icanhazip.com"
alias localip="ipconfig getifaddr en0" # macOS

# KUBERNETES
alias k=kubectl
alias kgp='kubectl get pods' # list all pods in the current namespace
alias kgs='kubectl get svc' # list all services in the current namespace
alias kd='kubectl describe' # get detailed information about a resource
alias krm='kubectl delete' # delete a specified resource
alias kga='kubectl get all' # list all resources in the current namespace
alias kr='kubectl run' # help you quickly start a new pod
alias kaf='kubectl apply -f' # apply the configuration from a YAML file
alias kgn='kubectl config get-contexts' # display your current context information which is really useful when dealing with multiple clusters
alias kcuc='kubectl config use-context' # helps in quickly switching the context

# macOS
alias showhidden="defaults write com.apple.Finder AppleShowAllFiles true && killall Finder"
alias hidehidden="defaults write com.apple.Finder AppleShowAllFiles false && killall Finder"

