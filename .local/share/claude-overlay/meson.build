project('claude-overlay', 'c',
  version: '0.1.0',
  default_options: ['c_std=c11', 'warning_level=2'],
)

cc = meson.get_compiler('c')

# Dependencies
dep_wayland_client = dependency('wayland-client')
dep_wayland_protocols = dependency('wayland-protocols')
dep_cairo = dependency('cairo')
dep_pangocairo = dependency('pangocairo')
dep_json_c = dependency('json-c')
dep_rt = cc.find_library('rt')

# Wayland scanner for protocol code generation
wayland_scanner = find_program('wayland-scanner', native: true)

wayland_scanner_code = generator(wayland_scanner,
  output: '@BASENAME@-protocol.c',
  arguments: ['private-code', '@INPUT@', '@OUTPUT@'],
)

wayland_scanner_header = generator(wayland_scanner,
  output: '@BASENAME@-protocol.h',
  arguments: ['client-header', '@INPUT@', '@OUTPUT@'],
)

# Protocol sources
protocol_dir = meson.current_source_dir() / 'protocols'

protocols = [
  protocol_dir / 'wlr-layer-shell-unstable-v1.xml',
]

# Also need xdg-shell from wayland-protocols
wl_protocol_dir = dep_wayland_protocols.get_variable('pkgdatadir')
protocols += [
  wl_protocol_dir / 'stable/xdg-shell/xdg-shell.xml',
]

src_protocols = []
foreach p : protocols
  src_protocols += wayland_scanner_code.process(p)
  src_protocols += wayland_scanner_header.process(p)
endforeach

# Main executable
executable('claude-overlay',
  'src/main.c',
  'src/wayland.c',
  'src/shm.c',
  src_protocols,
  dependencies: [
    dep_wayland_client,
    dep_cairo,
    dep_pangocairo,
    dep_json_c,
    dep_rt,
  ],
  install: true,
)
