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
    --color='bg:-1,bg+:#121220,fg:#505068,fg+:#d0d0e0'
    --color='hl:#d0b850,hl+:#d0b850'
    --color='info:#404040,marker:#50a060,spinner:#d0b850'
    --color='prompt:#d09040,pointer:#d09040'
    --color='header:#d0b850,header:bold'
    --color='border:#d09040,label:#d0b850,query:#d0d0e0'
    --color='separator:#282840'
)
