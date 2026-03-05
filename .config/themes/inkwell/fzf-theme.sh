# Shared fzf theme for foot+fzf picker TUIs
# Source this, then: fzf "${FZF_PICKER_OPTS[@]}" --border-label=' Label ' ...

FZF_PICKER_OPTS=(
    --height=100%
    --layout=reverse
    --no-sort
    --exact
    --no-scrollbar
    --info=hidden
    --border=rounded
    --pointer='▸'
    --marker='●'
    --separator='─'
    --color='bg:-1,bg+:#1c1926,fg:#706b7a,fg+:#d0ccd5'
    --color='hl:#6a8a9a,hl+:#6a8a9a'
    --color='info:#404040,marker:#6a9a74,spinner:#6a8a9a'
    --color='prompt:#9a7098,pointer:#9a7098'
    --color='header:#6a8a9a,header:bold'
    --color='border:#9a7098,label:#6a8a9a,query:#d0ccd5'
    --color='separator:#383344'
)
