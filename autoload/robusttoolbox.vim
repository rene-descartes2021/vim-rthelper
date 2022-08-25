" Copyright (c) 2022 Rene.Descartes2021


function! robusttoolbox#regen() abort
	let tf = b:gutentags_files.ctags
	let t1 = reltime()
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing implicit [DataDefinition]s')
	let i = robusttoolbox#get_ImplicitDataDefinitions(tf)
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing explicit [DataDefinition]s')
	let e = robusttoolbox#get_ExplicitDataDefinitions(tf)
	let ts = reltimestr(reltime(t1))
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing [DataDefinition]s completed in '.ts.' seconds')
endfunction

function! robusttoolbox#filtertags(tf, f)
	let ret = systemlist('readtags -t '.a:tf." -eE -Q '".a:f."' -l")
	if v:shell_error
		echohl ErrorMsg
		call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] readtags error: '.v:shell_error.' filter:'''.a:f.'''')
		echohl None
	endif
	return ret
endfunction

"Parallel example:
"readtags -t home-browser-dev-space-station-14-.tags -eE -Q '(and $kind (eq? $kind "C") (eq? ($"sl") "()") (#/(^\(|,) ?Component(,|\)$)/ $inherits))' -l | cut -f 1 | xargs -P0 -I {hi} readtags -t home-browser-dev-space-station-14-.tags -eE -Q '(and $kind (eq? $kind "C") (eq? ($"sl") "(sealed)") (#/(^\(|,) ?{hi}(,|\)$)/ $inherits))' -l

" Gets all [ImplicitDataDefinitionForInheritors] classes in a list
" Top level list is [ImplicitDataDefinitionForInheritors] list
" Any Children will be in 7th element, they implicitly are [DataDefinition]
" Children must be navigated recursively
"  [0..5 6]
"  {cname:{str:str datafields:{} children:{}} cname:{str:str datafields:{} children:{}} ...}
function! robusttoolbox#get_ImplicitDataDefinitions(tf)
	" Get [ImplicitDataDefinitionForInheritors] classes, all are abstract
	let roots = robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/\[ImplicitDataDefinitionForInheritors\]/ ($"at")))'))

	call map(roots, {i,j -> j[4] !=# 'sealed' ? add(j, robusttoolbox#get_Children(a:tf, j[0])): j})

	return roots
endfunction

" Gets all [DataDefinition] classes in a list, no children
function! robusttoolbox#get_ExplicitDataDefinitions(tf)
	return robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/\[DataDefinition\]/ ($"at")))')) +
	\ robusttoolbox#splitup_struct(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "S") (#/\[DataDefinition\]/ ($"at")))'))
endfunction

" For kind:C puts into [n, f, l, attributes, inherits, sealed/abstract]
function! robusttoolbox#splitup_class(list)
	" Is Extended Posix Regex, so can't match nested parens IIRC
	"echo matchlist(root[0], '\(\h\w\+\)\t\([^\t]\+\)\t\([^\t]\+\)\tkind:C\tat:\(([^\t]*)\)\t\inherits:(\([^)]*\))')\tsl:(\([^)]*\))[1:-4]

	return map(a:list, {i,j -> matchlist(j, '\(\h\w\+\)\t\([^\t]\+\)\t\([^\t]\+\)\tkind:C\tat:\(([^\t]*)\)\t\inherits:(\([^)]*\))\tsl:(\([^)]*\))')[1:-4]}) 
endfunction

" For kind:S puts into [n, f, l, attributes, inherits]
function! robusttoolbox#splitup_struct(list)
	" Is Extended Posix Regex, so can't match nested parens IIRC
	"echo matchlist(root[0], '\(\h\w\+\)\t\([^\t]\+\)\t\([^\t]\+\)\tkind:S\tat:\(([^\t]*)\)\t\inherits:(\([^)]*\))')[1:-4]

	return map(a:list, {i,j -> matchlist(j, '\(\h\w\+\)\t\([^\t]\+\)\t\([^\t]\+\)\tkind:S\tat:\(([^\t]*)\)\t\inherits:(\([^)]*\))')[1:-5]}) 
endfunction


function! robusttoolbox#get_Children(tf, tag)
	let children = robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/(^\(|,)'.a:tag.'(,|\)$)/ $inherits))'))

	return map(children, {i,j -> j[4] !=# 'sealed' ? add(j, robusttoolbox#get_Children(a:tf, j[0])): j})
endfunction


" Gets all [Prototype] references {entity, shader}
function! s:get_Prototypes()
endfunction
