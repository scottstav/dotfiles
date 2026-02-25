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
    --color='bg:-1,bg+:#1a2a40,fg:#808080,fg+:#e0e0e0'
    --color='hl:#00bfff,hl+:#89cff0'
    --color='info:#404040,marker:#00ff7f,spinner:#00bfff'
    --color='prompt:#1e90ff,pointer:#1e90ff'
    --color='header:#00bfff,header:bold'
    --color='border:#1e90ff,label:#89cff0,query:#e0e0e0'
    --color='separator:#303050'
)
