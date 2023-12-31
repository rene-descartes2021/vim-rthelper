" Copyright (c) 2022 Rene.Descartes2021

" This plugin directs vim-gutentags to make tags for cs and yaml files
" A json schema is then generated for RobustToolbox prototypes
" Query tagsfile g:gutentags_cache_dir/g:gutentags_ctags_tagfile
" e.g. ~/.config/tags/.tags
let g:rthelper_enabled = get(g:, 'rthelper_enabled', v:true)
if !g:rthelper_enabled
	finish
endif

augroup RTHELPER_VIM
	au!
	au BufEnter *.cs,*.yml,*.yaml command! -buffer -bang -bar -nargs=0 RTParse call rthelper#ParseData(<bang>0)
	au BufEnter *.cs,*.yml,*.yaml command! -buffer -bar -nargs=? RTGenSchema call rthelper#GenSchema(<q-args>)
	au BufEnter *.cs,*.yml,*.yaml nnoremap <buffer> <Plug>(rthelper_parse) :RTParse<CR>
	au BufEnter *.cs,*.yml,*.yaml nnoremap <buffer> <Plug>(rthelper_gen_schema) :RTGenSchema<CR>
augroup END

let g:rthelper_use_customTags = get(g:, 'rthelper_use_customTags', v:false)
let g:rthelper_run_on_tags_updated = get(g:, 'rthelper_run_on_tags_updated', v:true)
if g:rthelper_run_on_tags_updated
	au RTHELPER_VIM User GutentagsUpdated call rthelper#GenSchema()
endif

" vim-denops {
	"let g:denops#debug = 1
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

	"let g:gutentags_trace = 1
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
	" asyncomplete is plugin that goes with vim-lsp
	let g:asyncomplete_auto_popup = 1

	let g:lsp_log_file = fnamemodify(tempname(), ':h').'/vim-lsp.log'

	let in_dir = (exists('*gutentags#get_project_root') ?
		\ gutentags#get_project_root(getcwd()) : getcwd()) . '/Resources/Schemas'
	if g:rthelper_use_customTags && isdirectory(in_dir)
		let in_file = in_dir.'/customTags.json'
		let customTags = filereadable(in_file) ?
			\ json_decode(join(readfile(in_file, 'b')))['customTags'] : []
	else
		let customTags = []
	endif

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
	\		 'customTags': customTags,
	\  }
	\ }
	\}
	"\		 'customTags': [ '!type:SetFloatOperator mapping' ],
	"\ 'disabled': 1,
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
