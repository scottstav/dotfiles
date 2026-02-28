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
    --color='bg:-1,bg+:#1a2a40,fg:#8a9a8a,fg+:#d4d4d4'
    --color='hl:#60b0a0,hl+:#60b0a0'
    --color='info:#404040,marker:#50c878,spinner:#60b0a0'
    --color='prompt:#50c878,pointer:#50c878'
    --color='header:#60b0a0,header:bold'
    --color='border:#50c878,label:#60b0a0,query:#d4d4d4'
    --color='separator:#303050'
)
