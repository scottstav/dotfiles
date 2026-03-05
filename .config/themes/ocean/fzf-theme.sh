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
    --color='bg:-1,bg+:#1a1e24,fg:#6e7681,fg+:#e0e0e0'
    --color='hl:#00bfff,hl+:#00bfff'
    --color='info:#404040,marker:#00ff7f,spinner:#00bfff'
    --color='prompt:#1e90ff,pointer:#1e90ff'
    --color='header:#00bfff,header:bold'
    --color='border:#1e90ff,label:#00bfff,query:#e0e0e0'
    --color='separator:#3a3a4a'
)
