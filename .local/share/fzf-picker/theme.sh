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
    --color='bg:-1,bg+:#252a24,fg:#8a8070,fg+:#d4d4d4'
    --color='hl:#6a9e8e,hl+:#6a9e8e'
    --color='info:#404040,marker:#5b9a6a,spinner:#6a9e8e'
    --color='prompt:#5b9a6a,pointer:#5b9a6a'
    --color='header:#6a9e8e,header:bold'
    --color='border:#5b9a6a,label:#6a9e8e,query:#d4d4d4'
    --color='separator:#5a4a3a'
)
