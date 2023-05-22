" Copyright (c) 2022 Rene.Descartes2021

" This plugin directs vim-gutentags to make tags for cs and yaml files
" A json schema is then generated for RobustToolbox prototypes
" Query tagsfile g:gutentags_cache_dir/g:gutentags_ctags_tagfile
" e.g. ~/.config/tags/.tags

augroup ROBUST_TOOLBOX_VIM
	au!
	au BufEnter *.cs,*.yml,*.yaml command! -buffer -bar -nargs=0 RTParse call robusttoolbox#ParseData()
	au BufEnter *.cs,*.yml,*.yaml command! -buffer -bar -nargs=0 RTGenSchema call robusttoolbox#GenSchema()
	au BufEnter *.cs,*.yml,*.yaml nnoremap <buffer> <Plug>(robusttoolbox_parse) :RTParse<CR>
	au BufEnter *.cs,*.yml,*.yaml nnoremap <buffer> <Plug>(robusttoolbox_gen_schema) :RTGenSchema<CR>
	"au User GutentagsUpdated call robusttoolbox#ParseData()
augroup END

" vim-denops {
	let g:denops#debug = 1
" }

" vim-gutentags {
	" Ensure vim-gutentags loaded on relevant filetypes
	"  vim-plug isn't extensible like this, up to plugin-consumer to
	"  configure before plug#end(), though perhaps BufEnter could be used?
	" Once this plugin works in background thread I can work out events
	if exists('*dein#get')
		call uniq(sort(extend(dein#get('vim-gutentags').on_ft, ['cs', 'yaml'])))
	endif

	let ctags_exclude_file = fnamemodify(expand('<sfile>'), ':h').'/../data/exclude.ctags'
	let ctags_cs_file = fnamemodify(expand('<sfile>'), ':h').'/../data/cs.ctags'
	let ctags_yaml_file = fnamemodify(expand('<sfile>'), ':h').'/../data/yaml.ctags'

	let g:gutentags_ctags_extra_args = get(g:, 'gutentags_ctags_extra_args', [])

	let g:gutentags_ctags_extra_args += [
		\ '--exclude=@'.ctags_exclude_file,
		\ '--options='.ctags_cs_file,
		\ '--options='.ctags_yaml_file,
		\ ]

	" ctags file input field pattern depends on absolute or relative
	"  'search start path' when invoking ctags. Appears ctags is invoked
	"  absolute by vim-gutentags when tags file not in project dir.
	" Relative paths would be preferrable to absolute, but tags in tmpfs
	"  even more preferrable. Optimal solution will be to modify
	"  vim-gutentags to symlink project tag file into tmpfs cache dir.
	"  Can make PR for #176.
	" See: https://github.com/universal-ctags/ctags/issues/759
	"			 https://github.com/ludovicchabant/vim-gutentags/issues/176
" }

" vim-lsp {
	" Multiple server instances seem to append into one log fine:
	let g:lsp_log_file = fnamemodify(tempname(), ':h').'/vim-lsp.log'
	"let g:gutentags_trace = 1

	let g:asyncomplete_auto_popup = 1
	let g:lsp_settings = get(g:, 'lsp_settings', {})
	let g:lsp_settings['yaml-language-server'] = {
	\ 'workspace_config': {
	\		'yaml': {
	\		 'schemas': {
	\			 './Resources/Schemas/prototypes.json': '/Resources/Prototypes/**/*.yml',
	\		 },
	\		 'completion': v:true,
	\		 'hover': v:true,
	\		 'validate': v:true,
	\		 'trace': { 'server': 'debug' },
	\		 'customTags': [ '!type:SetFloatOperator mapping' ],
	\  }
	\ }
	\}
	"\ 'disabled': 1,
	"\		'customTags': [],
	"\		'schemaStore': { 'enable': v:true },

	" Some notes on vim-lsp and notifying yaml-language-server of schema change:
	" To validate schema/workspace was applied:
	"  get b:lspinfo with hotkey: `<space>lXdI`
	"  echo filter(copy(b:lsp_info.workspace_config.yaml.schemas), {k,v -> k =~# '^note'})

	" Strange, not in above, yet it is in g:lsp_settings!!!
	"let s = g:lsp_settings['yaml-language-server'].yaml.schemas
	"echo filter(copy(s), {k,v -> k =~# '^note'})

	"call lsp#update_workspace_config('yaml-language-server', g:lsp_settings['yaml-language-server'].workspace_config)
	" call lsp#send_request('yaml-language-server', { 'method': 'yaml/get/jsonSchema', 'params': expand('%')})
" }
