" Copyright (c) 2022 Rene.Descartes2021

let tempdir = fnamemodify(tempname(), ':h')
let ctags_exclude_file = tempdir.'/ctags_exclude'
" Multiple server instances seem to append into one log fine:
let g:lsp_log_file = '/tmp/vim-lsp.log'
"let g:lsp_log_file = tempdir.'/vim-lsp.log'
let g:gutentags_trace = 1

let g:asyncomplete_auto_popup = 1
"let g:polyglot_disabled = ['autoindent']
autocmd BufEnter *.yml set indentexpr=

" Can't write to that file from plugin/*.vim:
"get(g:, 'gutentags_cache_dir', expand('~/.config/tags')).'/excludes'

" ctags_exclude pattern depends on absolute or relative
"  'search start path' when invoking ctags. Appears ctags is invoked
"  absolute by vim-gutentags when tags file not in project dir.
" Relative paths would be preferrable to absolute, but tags in tmpfs even
"  more preferrable. Optimal solution will be to modify vim-gutentags to
"  symlink project tag file into tmpfs cache dir. Can make PR for #176.
" See: https://github.com/universal-ctags/ctags/issues/759
"      https://github.com/ludovicchabant/vim-gutentags/issues/176
let ctags_exclude_data = [
  \ '*/obj/*',
  \ '*/bin/*',
  \ '*/bin.bak/*',
  \ '*/BuildChecker/*',
  \ '*/BuildFiles/*',
  \ 'Makefile',
  \ '*.css',
  \ '*.ctags',
  \ '*.bat',
  \ '*.h',
  \ '*.html',
  \ '*.js',
  \ '*.json',
  \ '*.m',
  \ '*.md',
  \ '*.svg',
  \ '*.ps1',
  \ '*.py',
  \ '*.xml',
  \ ]

if !filereadable(ctags_exclude_file)
  call writefile(ctags_exclude_data, ctags_exclude_file, 'S')
endif

let g:gutentags_ctags_extra_args = get(g:, 'gutentags_ctags_extra_args', [])

let g:gutentags_ctags_extra_args += [
  \ '--exclude=@'.ctags_exclude_file,
  \ '--extras=+p-q',
  "\ '--extras=+pq',
  \ '--pseudo-tags=+{TAG_EXTRA_DESCRIPTION}{TAG_KIND_DESCRIPTION}{TAG_FIELD_DESCRIPTION}{TAG_PROC_CWD}',
  \ ]

"let g:gutentags_ctags_extra_args += ['--fields=+k']


let ctags_cs_file = tempdir.'/cs.ctags'
let ctags_csharp_file = tempdir.'/csharp.ctags'
" Custom cs language using regex tables, seems slower
let ctags_cs_data = [
  \ '--langdef=cs{_autoFQTag}',
  "\ '--map-c#=-.cs',
  "\ '--map-cs=+.cs',
  "\ '--kinds-c#=d',
	\ '--_fielddef-cs=at,Attributes',
  \ '--_fielddef-cs=sl,sealed',
  \ '--_fielddef-cs=inherits,inherits',
  \ '--_fielddef-cs=type,type',
  \ '--_fielddef-cs=df,default',
  \ '--kinddef-cs=N,anamespace,Namespaces',
  \ '--kinddef-cs=C,aclass,Classes',
  \ '--kinddef-cs=S,astruct,Structures',
  \ '--kinddef-cs=M,amember,Members',
  \ '--kinddef-cs=P,aproperty,Properties',
  \ '--kinds-cs=+CSMP',
  \ '--fields-cs=+{at}{inherits}{sl}{type}{df}',
  \ '--_tabledef-cs=main',
  \ '--_tabledef-cs=namespace',
  \ '--_tabledef-cs=class',
  \ '--_tabledef-cs=comment',
  \ '--_tabledef-cs=structure',
  \ '--_tabledef-cs=function',
  \ '--_mtable-regex-cs=main/namespace\h([^\n{;\h]+)(?:\h[{]|\n[{]|;)/\1/N/p{scope=push}{tenter=namespace}',
  \ '--_mtable-regex-cs=main/[^\n]+\n|[^\n]+|\n//p',
  \ '--_mtable-regex-cs=main/[}]//p{scope=clear}{tquit}',
  \ '--_mtable-regex-cs=namespace/\bclass\b\h+([^\n]+)\n/\1/C/p{scope=push}{tenter=class}',
  \ '--_mtable-regex-cs=namespace/\n[}]//p{scope=pop}{tleave}',
  \ '--_mtable-regex-cs=namespace/.//p',
  \ '--_mtable-regex-cs=comment/\*\///p{tleave}',
  \ '--_mtable-regex-cs=comment/.//p',
  \ '--_mtable-regex-cs=class/[}]//p{tleave}',
  \ '--_mtable-regex-cs=class/\/\*//p{tenter=comment}',
  \ '--_mtable-regex-cs=class/[{]//p{tenter=function}',
  \ '--_mtable-regex-cs=class/.//p',
  \ '--_mtable-regex-cs=function/[}]//p{tleave}',
  \ '--_mtable-regex-cs=function/\/\*//p{tenter=comment}',
  \ '--_mtable-regex-cs=function/[{]//p{tenter=function}',
  \ '--_mtable-regex-cs=function/.//p',
  \ ]

if !filereadable(ctags_cs_file)
  call writefile(ctags_cs_data, ctags_cs_file, 'S')
endif

" --mline-regex, should be slower than regex tables
"  But is faster for now in my testing
let ctags_csharp_data = [
  "\ '--kinds-c#=imdnegtEpsZc',
  \ '--kinds-c#=deEgit',
	\ '--fields=+stkZ-aiSfl',
  \ '--_fielddef-c#=at,Attributes',
  \ '--_fielddef-c#=sl,sealed',
  \ '--_fielddef-c#=inherits,inherits',
  \ '--_fielddef-c#=type,type',
  \ '--_fielddef-c#=df,default',
  \ '--kinddef-c#=N,anamespace,Namespaces',
  \ '--kinddef-c#=C,aclass,Classes',
  \ '--kinddef-c#=S,astruct,Structures',
  \ '--kinddef-c#=M,amember,Members',
  \ '--kinddef-c#=P,aproperty,Properties',
  \ '--kinds-c#=+NCSMP',
  \ '--fields-c#=+{at}{inherits}{sl}{type}{df}',
  \ '--regex-c#=/^namespace\h([[:alpha:]][\w\.]++)/\1/N/p{scope=push}',
  \ '--mline-regex-c#=/^\h*+((?:\[[^][]+]\s?\h*+)*+)(?:(?:public|private|protected|internal)\h)?(?:(sealed|abstract)\h)?(?:unsafe\h)?(?:partial\h)?class\h([[:alpha:]]\w++)(?:\h:\h([^{\s]*+))?\s?\h*+[{]\s/\3/C/p{_field=at:(\1)}{_field=inherits:(\4)}{_field=sl:(\2)}{mgroup=3}{{',
  \ '  _scopedepth 0 gt {',
  \ '    _scopetop . scope:',
  \ '  } if',
  \ '  . _scopepush',
  \ '}}',
  \ '--mline-regex-c#=/^\h*+((?:\[[^][]+]\s?\h*+)*+)(?:(?:public|private|protected|internal)\h)?(?:unsafe\h)?struct\h([[:alpha:]]\w++)\h(?::\h([^{\s]*+))\s?\h*+[{]\s/\2/S/p{scope=push}{scope=ref}{_field=at:(\1)}{_field=inherits:(\3)}{mgroup=2}',
  \ '--mline-regex-c#=/^\h*+((?:\[[^][]+]\s?\h*+)*+)(?:\b(?:public|private|protected|internal|virtual|static|override|readonly)\b\h)*+([\w<>]++[?]?)\h(\b[[:alpha:]]\w++\b)(?:\h=[>]?\s?\h*+([^;]++))?;/\3/M/p{_field=at:(\1)}{_field=type:\2}{_field=df:\4}{mgroup=3}{scope=ref}',
  \ '--mline-regex-c#=/^\h*+((?:\[[^][]+]\s?\h*+)*+)(?:\b(?:public|private|protected|internal|vіrtual|static|override|readonly)\b\h)*+([\w<>]++[?]?)\h(\b[[:alpha:]]\w++\b)\s?\h*+(?<brak>{(?:[^}{]*+(?&brak)?)*+})(?:\h=[>]?\h([^;]++);)?/\3/P/p{_field=at:(\1)}{_field=type:\2}{_field=df:(\5)}{mgroup=3}{scope=ref}',
  \ '--regex-c#=/^\h*+[}]///p{scope=pop}{exclusive}',
  \ ]

if !filereadable(ctags_csharp_file)
  call writefile(ctags_csharp_data, ctags_csharp_file, 'S')
endif

let g:gutentags_ctags_extra_args += [
  \ '--options='.ctags_cs_file,
	\ ]

"P doesn't capture Component.NetSyncEnabled default
"C doesn't capture sealed
" Down to 4secs thanks to limiting to 1 newline and removing prefix .*
" Possibly lost some classes/structs when removing prefix .*
" as filesize went down 5K even though sealed field added
" 20181K->20176K
" Now up to 20838K with removal of .+ instances, and speed <3s
" Now with force of P default, NetSyncEnabled eats subsequent Owner and LifeStage??? and uses the LifeStage default... crazy.
" Disallow \s before ; for now to stop that greedy eating.
" Ok I need to allow sub {set; get{}} in P:
"  (?<brak>{(?:[^}{]*(?&brak)?)*+})
" Now it is 20840K with speed ~2s, but missing P without defaults.
" Now 21717K with P with/without defaults! ~2s.
" This good website: https://regex101.com/r/eBtSTM/2
"\ '--kinddef-c#=U,ausing,Usings',
"\ '--regex-c#=/^using\h([^;]+);/\1/U/p{exclusive}',
" Use \b for word boundaries, for speedup
" Use *+ and ++ for 'possessive' greedy match over * and +, for speedup
" possessіve greedy match sped up --kinds-c#=+CSMP 4s->1.8s !!!! yay!!!
" \b \b in M and P did 1.8s -> 1.75s, I should read more on that.

"\     'schemas': { '/home/browser/dev/space-station-14/notes/YAMLSchemas/schemas/prototypes.json': 'Resources/Prototypes/*.yml' },

let g:lsp_settings = get(g:, 'lsp_settings', {})
let g:lsp_settings['yaml-language-server'] = {
  \ 'workspace_config': {
  \   'yaml': {
  \    'schemas': {
  \      'https://mattn.github.io/efm-langserver/schema.json': '/efm-langserver/config.yaml',
  \      './notes/YAMLSchemas/schemas/prototypes.json': '/Resources/Prototypes/**/*.yml',
  \    },
  \    'completion': v:true,
  \    'hover': v:true,
  \    'validate': v:true,
  \    'trace': { 'server': 'debug' }
  \  }
  \ }
  \}
  "\    'customTags': [],
  "\      'file:///home/browser/dev/space-station-14/notes/YAMLSchemas/schemas/prototypes.json': '/Resources/Prototypes/**/*.yml'
  "\      './notes/YAMLSchemas/schemas/prototypes.json': '/Resources/Prototypes/**/*.yml',
  "\    'schemaStore': { 'enable': v:true },

" To validate my schema/workspace was applied:
"  get b:lspinfo with hotkey: `<space>lXdI`
"  echo filter(copy(b:lsp_info.workspace_config.yaml.schemas), {k,v -> k =~# '^note'})

" Strange, not in above, yet it is in g:lsp_settings!!!
"let s = g:lsp_settings['yaml-language-server'].yaml.schemas
"echo filter(copy(s), {k,v -> k =~# '^note'})

"call lsp#update_workspace_config('yaml-language-server', g:lsp_settings['yaml-language-server'].workspace_config)
" call lsp#send_request('yaml-language-server', { 'method': 'yaml/get/jsonSchema', 'params': expand('%')})

function! s:update_yaml_language_server()
  call lsp#update_workspace_config('yaml-language-server', workspace_config)
endfunction

"call s:update_yaml_language_server()

function! Schemagen()
  " Query tagsfile g:gutentags_cache_dir/g:gutentags_ctags_tagfile
  " e.g. ~/.config/tags/.tags
  let tagsfile = get(g:, 'gutentags_cache_dir', expand('~/.config/tags')).'/'.get(g:, 'gutentags_ctags_tagfile', '.tags')
  call execute('readtags', query)
  call s:update_yaml_language_server()
endfunction
