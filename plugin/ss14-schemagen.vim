" Copyright (c) 2022 Rene.Descartes2021

let tempdir = fnamemodify(tempname(), ':h')
" Multiple server instances seem to append into one log fine:
let g:lsp_log_file = '/tmp/vim-lsp.log'
"let g:lsp_log_file = tempdir.'/vim-lsp.log'
"let g:gutentags_trace = 1

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
"			 https://github.com/ludovicchabant/vim-gutentags/issues/176
let ctags_exclude_file = fnamemodify(expand('<sfile>'), ':h').'/../data/exclude.ctags'
let ctags_cs_file = fnamemodify(expand('<sfile>'), ':h').'/../data/cs.ctags'
let ctags_yaml_file = fnamemodify(expand('<sfile>'), ':h').'/../data/yaml.ctags'

let g:gutentags_ctags_extra_args = get(g:, 'gutentags_ctags_extra_args', [])

let g:gutentags_ctags_extra_args += [
	\ '--exclude=@'.ctags_exclude_file,
	\ '--options='.ctags_cs_file,
	\ '--options='.ctags_yaml_file,
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

"\		 'schemas': { '/home/browser/dev/space-station-14/notes/YAMLSchemas/schemas/prototypes.json': 'Resources/Prototypes/*.yml' },

"TODO: Why did I have this schema below? I didn't remove from copy-paste I guess:
" \			 'https://mattn.github.io/efm-langserver/schema.json': '/efm-langserver/config.yaml',

let g:lsp_settings = get(g:, 'lsp_settings', {})
let g:lsp_settings['yaml-language-server'] = {
	\ 'workspace_config': {
	\		'yaml': {
	\		 'schemas': {
	\			 './notes/YAMLSchemas/schemas/gen/prototypes.json': '/Resources/Prototypes/**/*.yml',
	\		 },
	\		 'completion': v:true,
	\		 'hover': v:true,
	\		 'validate': v:true,
	\		 'trace': { 'server': 'debug' }
	\  }
	\ }
	\}
	"\		'customTags': [],
	"\			'file:///home/browser/dev/space-station-14/notes/YAMLSchemas/schemas/prototypes.json': '/Resources/Prototypes/**/*.yml'
	"\			'./notes/YAMLSchemas/schemas/prototypes.json': '/Resources/Prototypes/**/*.yml',
	"\		'schemaStore': { 'enable': v:true },

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
