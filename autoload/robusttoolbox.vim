" Copyright (c) 2022 Rene.Descartes2021

" Builds JSON schema for SS14 YAML prototypes
" Parses ctags file `b:gutentags_files.ctags` using readtags
" Uses vimscript. Output is list of dictionaries representing each
"  [DataDefinition] class/structure.
" ctags generation takes ~2s for whole SS14 project
"  Optimization would be to use regex tables, could be ~0.2s for whole
"  vim-gutentags regens tags subset on BufWrite event, faster than whole
" Then this schema regen takes 12.39s on my system for just [DataDef..]
"  Many readtags serial readtags queries and processed in vimscript
"  Optimization matchlist() ~8x faster than split()
"  Optimization would be to process data with pipes and perl/rust regex
"  Optimization would be to re-write in lua or rust
"  Optimization would be to write subschemas instead of whole on BufWrite
"		however yaml-language-server doesn't support subschemas at present
"  Consider optimizations in context of sharing backend between IDEs

" Parallel shell example:
"  Intermediate data are lost, necessary for output data structure
"  Consider hierarchy of [DataDefinition] and inheritance of [DataField]
"  Could possibly build flat list then process hierarchy later when
"		querying [DataField]s, or process hierarchy after than via inherits?
"  Note parallel shell technique may not readily translate to MS Windows
"readtags -t home-browser-dev-space-station-14-.tags -eE -Q '(and $kind (eq? $kind "C") (eq? ($"sl") "()") (#/(^\(|,) ?Component(,|\)$)/ $inherits))' -l | cut -f 1 | xargs -P0 -I {hi} readtags -t home-browser-dev-space-station-14-.tags -eE -Q '(and $kind (eq? $kind "C") (eq? ($"sl") "(sealed)") (#/(^\(|,) ?{hi}(,|\)$)/ $inherits))' -l

let s:self = expand('<sfile>')
command! -nargs=0 RobustResource call execute('source '.s:self)

function! robusttoolbox#regen() abort
	let tf = b:gutentags_files.ctags
	let t1 = reltime()
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing [Prototype]s')
	let p = robusttoolbox#get_Prototypes(tf)
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing explicit [DataDefinition]s')
	let e = robusttoolbox#get_ExplicitDataDefinitions(tf)
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing implicit [DataDefinition]s')
	let i = robusttoolbox#get_ImplicitDataDefinitions(tf)
	let ts = reltimestr(reltime(t1))
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing [Prototype]s completed in '.ts.' seconds')
	return {'i':i, 'e':e, 'p':p}
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

" Gets all [ImplicitDataDefinitionForInheritors] class tags in a list of lists of dictionaries
"  Each sublist element representing a class tag
"  Each sublist a set of class tags for a class (i.e. partial)
"  Top level list is [ImplicitDataDefinitionForInheritors] list
"  Any child classes will be in 'c' element of first class tag in set
"  Children must be navigated recursively
"  [{n:name s:scope i:'inherits list' a:'sealed|abstract' c:{...}, {...}...}]
"  'a' and 'c' key won't exist on structures
"  'c' key won't exist when no children
"  'a' key empty when not abstract or sealed
function! robusttoolbox#get_ImplicitDataDefinitions(tf)
	" TODO: try to do eq? instead of regex for speedup
	" Step 1: Query attribute tags
	let roots = robusttoolbox#splitup_attribute(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "A") (#/^\[ImplicitDataDefinitionForInheritors\]$/ $name))'))
	"let roots = robusttoolbox#splitup_attribute(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "A") (eq? $name "[ImplicitDataDefinitionForInheritors]"))'))
	" Step 2: for each attribute tag: query the class tags
	" Gets scope-name from attribute of class to match class scope
	let roots = map(map(roots, {i,j -> robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (eq? $name "'.j.t.'") (eq? $scope-name "'.matchlist(j.s, '\(.*\)\..*$')[1].'"))')}), {i,j -> robusttoolbox#splitup_class(j)})

	" Step 3: for each class get any child classes recursively
	"  'c' element only on first element of list of class tag dictionaries
	call map(roots, {i,j -> j[0].a !=# 'sealed' ? extend(j[0], {'c': robusttoolbox#get_Children(a:tf, j[0].n)}) : j[0]})
	return roots
endfunction

" Gets all [DataDefinition] classes in a list, no children
function! robusttoolbox#get_ExplicitDataDefinitions(tf)
	" Step 1: Query attribute tags
	let roots = robusttoolbox#splitup_attribute(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "A") (#/^\[DataDefinition\]$/ $name))'))

	" Step 2: for each attribute tag get the relevant class/struct tags
	return map(map(roots, {i,j -> robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "'.(j.k==#'class'?'C':(j.k==#'struct'?'S':'Undefined')).'") (eq? $name "'.j.t.'") (eq? $scope-name "'.matchlist(j.s, '\(.*\)\..*$')[1].'"))')}), {i,j -> robusttoolbox#splitup_class_struct(j)})

	" I could split Step 2 above into two queries: one for each kind!:
	"return robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/\[DataDefinition\]/ ($"at")))')) +
	"\ robusttoolbox#splitup_struct(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "S") (#/\[DataDefinition\]/ ($"at")))'))
endfunction

" Gets all [Prototype("...")] classes in a list, no children
" All are classes, no structs. No children???
" list of list of dicts
" But where to put the prototype parameters like name?
" Also see: [DataField("name", required=true, serverOnly=true,...)]
" Store the split up attribute first in sublist? Yes, why not!
function! robusttoolbox#get_Prototypes(tf)
	let t1 = reltime()
	"let p = robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/\[Prototype\(\"[[:alpha:]]+\"\)/ ($"at")))'))
	" Step 1: Query attribute tags
	let p = robusttoolbox#splitup_attribute(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "A") (#/^\[Prototype\(\"/ $name))'))

	" Step 2: For each attribute tag: query for the class tags 
	" Gets scope-name from attribute of class to match class scope
	call map(map(p, {i,j -> [j] + robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (eq? $name "'.j.t.'") (eq? $scope-name "'.matchlist(j.s, '\(.*\)\..*$')[1].'"))')}), {i,j -> map(j, {l,m -> l == 0 ? m : robusttoolbox#splitup_class_i(m)})})
	let ts = reltimestr(reltime(t1))
	call spacevim#vim#cursor#TruncatedEcho('[robusttoolbox] Parsing [Prototype]s completed in '.ts.' seconds')
	" 14+ seconds on first run, then ~1/3 time afterwards
	" 5.3 seconds
	" 5.285 without matchlist()[1:-5] indexing on splitup_class_i
	return p
	"return map(map(p, {i,j -> [j] + robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (eq? $name "'.j.t.'") (eq? $scope-name "'.matchlist(j.s, '\(.*\)\..*$')[1].'"))')}), {i,j -> type(j) == v:t_dict ? j : robusttoolbox#splitup_class(j)})
endfunction

" Gets all qualified [DataField] members/properties in a list
function! robusttoolbox#get_DataFields(tf)
	let m = robusttoolbox#splitup_member(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "M") (#/\[DataField\(\"/ ($"at")))'))
	let p = robusttoolbox#splitup_property(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "P") (#/\[DataField\(\"/ ($"at")))'))
	" TODO: filter results of the query to only include those with qualified tags, at present none appear to be qualified. Qualification appears to be necessary for scoping of which [DataField] applieѕ to which [DataDefinіtion]
	return [m, p]
endfunction

" For kind:A puts into {n:name, f:file, k:scope-kind, s:scope-name, t:typename}
function! robusttoolbox#splitup_attribute(list)
	" Is Extended Posix Regex, so can't match nested parens IIRC
	"  Appears to be no performance benefit of not capturing groups
	" For Class attributes should prune suffix of scope-name after this call
	" For Class members should leave scope-name as-is
	return map(map(a:list, {i,j -> matchlist(j, '\([^\t]\+\)\t\([^\t]\+\)\t[^\t]\+\tkind:A\tscope:\([^:]\+\):\([^\t]\+\)\ttyperef:typename:\([^\t]\+\)')[1:-5]}), {i,j -> {'n': j[0], 'f': j[1], 'k': j[2], 's': j[3], 't': j[4]}})
endfunction

" For kind:C puts into {n:name, k:scope-kind, s:scope-name, i:inherits, a:[sealed|abstract]}
function! robusttoolbox#splitup_class(list)
	return map(map(a:list, {i,j -> matchlist(j, '\(\w\+\)\t[^\t]\+\t[^\t]\+\tkind:C\tscope:\([^:]\+\):\([^\t]\+\)\tinherits:(\([^)]*\))\tsl:\(.*\)$')[1:-5]}), {i,j -> {'n': j[0], 'k': j[1], 's': j[2], 'i': j[3], 'a': j[4]}})
endfunction
function! robusttoolbox#splitup_class_i(str)
	let j = matchlist(a:str, '\(\w\+\)\t[^\t]\+\t[^\t]\+\tkind:C\tscope:\([^:]\+\):\([^\t]\+\)\tinherits:(\([^)]*\))\tsl:\(.*\)$')
	return {'n': j[1], 'k': j[2], 's': j[3], 'i': j[4], 'a': j[5]}
endfunction

" For kind:S puts into {n:name, k:scope-kind, s:scope-name, i:inherits}
function! robusttoolbox#splitup_struct(list)
	return map(map(a:list, {i,j -> matchlist(j, '\(\h\w\+\)\t[^\t]\+\t[^\t]\+\tkind:S\tscope:\([^:]\+\):\([^\t]\+\)\tinherits:(\([^)]*\))$')[1:-6]}), {i,j -> {'n': j[0], 'k': j[1], 's': j[2], 'i': j[3]}})
endfunction

" For kind:S or kind:C combo queries, slower than either
" Suitable for when an attribute applies to either, perhaps
" Should benchmark to see if separating first is faster
" Had to modify sl: capture to be optional to catch both struct and class
function! robusttoolbox#splitup_class_struct(list)	
	return map(map(a:list, {i,j -> matchlist(j, '\(\w\+\)\t[^\t]\+\t[^\t]\+\tkind:\(\a\)\tscope:\([^:]\+\):\([^\t]\+\)\tinherits:(\([^)]*\))\(\tsl:\([^$]*\)\)\?$')[1:-3]}), {i,j -> (j[1] ==# 'C' ? {'n': j[0], 'k': j[2], 's': j[3], 'i': j[4], 'a': j[6]} : {'n': j[0], 'k': j[2], 's': j[3], 'i': j[4]})})
endfunction

" For kind:M puts into {n:name, k:scope-kind, s:scope-name, t:type, d:default}
function! robusttoolbox#splitup_member(list)
	return map(map(a:list, {i,j -> matchlist(j, '\(\w\+\)\t[^\t]\+\t[^\t]\+\tkind:M\tscope:\([^:]\+\):\([^\t]\+\)\ttype:\([^\t]\+\)\tdf:\(.*\)$')[1:-5]}), {i,j -> {'n': j[0], 'k': j[1], 's': j[2], 't': j[3], 'd': j[4]}})
	" To test for mismatches look for 0:
	"return map(map(a:list, {i,j -> matchlist(j, '\(\w\+\)\t[^\t]\+\t[^\t]\+\tkind:M\tscope:\([^:]\+\):\([^\t]\+\)\ttype:\([^\t]\+\)\tdf:\(.*\)$')[1:-5]}), {i,j -> len(j)})
endfunction

" For kind:P puts into {n:name, k:scope-kind, s:scope-name, t:type, d:default}
"  Almost identical to splitup_member but regex compiled not to 'kind:M'
function! robusttoolbox#splitup_property(list)
	return map(map(a:list, {i,j -> matchlist(j, '\(\h\w\+\)\t[^\t]\+\t[^\t]\+\tkind:P\tscope:\([^:]\+\):\([^\t]\+\)\ttype:\([^\t]\+\)\tdf:\(.*\)$')[1:-5]}), {i,j -> {'n': j[0], 'k': j[1], 's': j[2], 't': j[3], 'd': j[4]}})
endfunction

function! robusttoolbox#get_Children(tf, tag)
	let children = robusttoolbox#splitup_class(robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (#/(^\(|,)'.a:tag.'(,|\)$)/ $inherits))'))
	" Now, for each child, we must get all class tag instances (i.e. partial)
	call map(map(children, {i,j -> robusttoolbox#filtertags(a:tf, '(and $kind (eq? $kind "C") (eq? $name "'.j.n.'") (eq? $scope "'.j.k.':'.j.s.'"))')}), {i,j -> robusttoolbox#splitup_class(j)})
	" List processing is slow sadly :(
	return map(children, {i,j -> j[0].a !=# 'sealed' ? [extend(j[0], {'c': robusttoolbox#get_Children(a:tf, j[0].n)})] + j[1:] : j})
endfunction
