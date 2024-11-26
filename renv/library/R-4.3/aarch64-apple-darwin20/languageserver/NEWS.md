# languageserver 0.3.16

**Closed issues:**

- Error in Neovim: “attempt to compare string with number” (#609)
- On-type formatter adds extra indentations after pipes (#607)

**Merged pull requests:**

- Handle vscode-notebook-cell in `path_from_uri` (#610)
- Skip incomplete lines in on-type-formatting (#615)
- Enable the folding of binary operations (#613)
- make sure column is defined (#614)
- Use code and codeDescription in diagnostic (#611)
- Fix path_from_uri (#612)
- Fix regex pattern to match chunk label after comma (#608)
- Include pacman Suggests dependency in DESCRIPTION (#604)
- Add link to R-Nova extension for Nova (#598)

# languageserver 0.3.15

**Closed issues:**
- package::function() autocompletion doesn't include re-exported functions (#590)
- Server failed to start probably due to hard-coded installation path ERROR (#579)

**Merged pull requests:**

- Include all exports in namespace objects (#591)
- Disable linters actions (#589)
- Avoid emitting a negative activeSignature (#586)
- fix CRAN check warnings (#583)
- Ignore error on checking is.function of namespace objects (#582)

# languageserver 0.3.14

**Closed issues:**

- NA introduced into position when opening curly braces (#564)
- `html-to-markdown.lua` error: attempt to call a nil value (#555)
- R6Class finalize() method is public, but it should be private as of R6 2.4.0 (#551)

**Merged pull requests:**

- Drop diagnostics on callback if disabled (#577)
- Bump styfle/cancel-workflow-action from 0.10.1 to 0.11.0 (#576)
- Bump styfle/cancel-workflow-action from 0.10.0 to 0.10.1 (#573)
- Provide package symbols before lint in diagnostics (#568)
- Support completion item labelDetails (#571)
- Enable multiple-level fold section of R document like RStudio (#566)
- fix NA introduced into position (#565)
- Requires lintr 3.0.0 (#562)
- Bump styfle/cancel-workflow-action from 0.9.1 to 0.10.0 (#560)
- Use lua-filter when pandoc_version >= 2.11 (#556)
- Update link to the lintr package (#554)
- Use private finalizers for R6 classes (#553)

# languageserver 0.3.13

**Closed issues:**

- Language Server crashing sporadically (#368)
- Why not set up r-universe? (#536)
- Signature help not show up in if-else branch (#530)
- Some of the tables of arguments are too complex to be represented by GFM (#533)
- Rmd detection ignores front matter (#507)
- Range formatting and onTypeFormatting are performed in Rmd non-code content (#224)
- Respect document language specified from editor (#510)
- "No newline at end of file" after formatting file in VS Code (#462)
- completionItem/resolve result may not be null (#504)
- Disabling linting has no effect (#497)

**Merged pull requests:**

- R 4.2 supports Unicode on Windows (#549)
- read may return -1 on error (#548)
- R universe (#542)
- do not pop the unbalance brackets in the current row (#541)
- Update GItHub Actions version (#537)
- Bump actions/upload-artifact from 1 to 3 (#540)
- Bump styfle/cancel-workflow-action from 0.9.0 to 0.9.1 (#539)
- Bump actions/cache from 2 to 3 (#538)
- convert the table of arguments to the sections of arguments with Lua filter (#534, thanks @atusy))
- Added an entry for BBEdit (#532)
- Support arg completion for primitive functions (#526)
- Check rmd scope in on type formatting (#525)
- Respect document `languageId` (#511)
- Document formatting ensures trailing empty line (#509)
- Only call `fs::path_abs` with existing files (#506)
- `completionItem/resolve` always returns `completionItem` (#505)
- Unpublish diagnostics when disabled (#498)
- add whitespace to NEWS.md (#490)

# languageserver 0.3.12

**Closed issues:**

- Documentation not working with R 4.1.1 (#468)
- Clean up dependencies (#470)
- Documentation for functions assigned with `=` differs from functions assigned with `<-` (#466)
- Bug when hovering variables (#463)
- Null workspace folder not working with untitled documents (#460)

**Merged pull requests:**

- Add installation steps of OS-specific dependencies (#247)
- Use internal `get_help` instead of repr (#469)
- Remove desc from imports (#471)
- Fix hover on function defined with `EQ_ASSIGN` (#467)
- Fix several bugs (#464)
- Fix handling null workspace folder and untitled document (#461)

# languageserver 0.3.11

**Closed issues:**

- documentation only loaded for packages named in a library() call, not with p_load() (#257)
- Minimal {styler} version requirement (#457)
- If load package via pacman::p_load(), the autocompletion will be not able to work well (#426)
- If I wrap library call in suppressPackageStartUpLanguages, autocompletion won't work (#451)
- Handle `workspace/didChangeWatchedFiles` (#446)
- Environment variable LANG (#435)
- Support lambda as function (#427)
- Support pipe operator in onTypeFormatting (#430)
- Exclusions in .lintr not working (#282)
- Packages not loaded prior to linting with coc-r-lsp (#238)
- Provide diagnostics on non-installed packages (#80)

**Merged pull requests:**

- Require styler >= 1.5.1 (#458)
- Support unscoped functions and library functions (#452)
- Handle `workspace/didChangeWatchedFiles` (#447)
- Add code action to disable linters (#408)
- Improve handling configuration settings (#439)
- check if lintr is new enough (#437)
- Use lint text if supported (#284)
- output text as is (#436)
- consolidate github action jobs (#434)
- Lambda support (#428)
- Use a function call as the completer in on-type-formatting (#431)
- support trace in InitializeParams (#423)

# languageserver 0.3.10

**Closed issues:**

- definitionProvider should work with file path (#415)
- Null value in documentation does not trigger signature (#417)
- Improve completion performance (#412)
- Completion not working before close parenthesis (#410)
- Use `styler::style_text(base_indention=)` for `styler > 1.4.0` (#400)
- Segfault on incorrect coordinates (#395)
- Case-insensitive and fuzzy match in completion (#401)
- Non-interactive setup of persistent {R.cache} cache for {styler} (#402)
- Document link should work with limited file extensions or file size (#391)
- get_document_symbol_kind error (#390)
- Support Call Hierarchy (#361)
- [FR] Display promise / active-binding objects in the OUTLINE (#362)

**Merged pull requests:**

- Definition provider works with file path (#416)
- Fix null doc_string in signature (#418)
- Faster completion (#413)
- fix raw string detection (#411)
- Use styler base_indention (#404)
- support R 4.0 raw string search (#276)
- fix a segfault issue (#407)
- Remove symbols starting with `._` from namespace (#406)
- Provide case-insensitive completion (#405)
- Update README.md (#403)
- fix missing whitespace (#399)
- Limit link file size (#393)
- Full testthat v3 compatibility (#394)
- Make headers in help hover look prettier (#392)
- Options completion (#389)
- Improve hover for function docs (#387)
- Implement Call Hierarchy provider (#373)
- Remove '(' from completion trigger chars (#374)
- Improve token completion (#372)
- Preserve argument order in argument completion (#370)
- Add '(' to completion trigger characters (#369)
- Use empty string instead of NULL as default signature doc (#371)
- Add hover to function symbol formals (#367)
- Implement selectionRangeProvider (#366)
- Recognize symbols created by delayedAssign / assign / makeActiveBinding (#364)
- use expect_setequal() may fit better (#363)

# languageserver 0.3.9

- skip tests on solaris

# languageserver 0.3.8

- When closing a file, "Problems" should be removed (#348)
- Implement renameProvider (#337)
- Hover on symbol in a function with functional argument causes parse error (#345)
- Hover on non-function symbol in other document should show definition and - documentation (#343)
- Check if symbol on rhs of assignment in definition (#341)
- Implement referencesProvider (#336)
- Add comment of notice above temp code of definition (#353)
- Implement renameProvider (#339)
- Fix tests (#351)
- Update coc help in README (#350)
- Remove diagnostics of closed files in non-package workspace (#349)
- Fix hover func arg (#346)
- Fix hover on non-function symbol (#344)
- Fix checking symbol on rhs of assignment expression (#342)
- Implement referencesProvider (#338)
- Do not load sysdata.rda (#347)
- release v0.3.7 (#335)

# languageserver 0.3.7

- Local function support (#330)
- Update xpaths to adapt to token change in parse data in R 4.0 (#328)
- Exclude existing completion items in token_completion (#326)
- Add token completion (#324)
- Only provide imported completions without package (#323)
- Improve folding (#317)
- More robust rmd chunk pattern (#318)
- Limit string length to provide color and link (#314)
- Minor improvements (#312)
- Improve xpath to work with cursor on token ending (#311)
- Support folding ranges for comments (#309)
- fix covr test (#310)
- Not trigger on-type-formatting on comment line (#308)
- Convert roxygen comments to documentation (#305)
- Support all symbols in definitions (#295)
- Use isf=FALSE (#303)
- Support startup library specified in profile (#302)
- Fix rmd chunk pattern (#297)
- Try parsing string in link and color (#300)
- Implement foldingRangeProvider (#294)
- Check uri in diagnostics_callback (#292)
- Fix for #283 - "unexpected '/'" on save (#291)
- use mac.binary instead of mac.binary.el-capitan (#293)
- Change dev version download script in the README (#287)
- Use sortText in completions (#286)
- Rmd chunk symbol (#280)
- make sure the path is UTF-8 (#278)
- make sure process is alive (#275)
- Fix handling raw string in link and color (#274)
- Indent all elements except braces in apply_initial_indention (#271)
- Use old behavior if formatting scope < indention (#269)
- Fix range formatting handling initial indentation (#268)
- Use more conservative pool_size (#267)
- add session pool for task manager (#265)

# languageserver 0.3.6

- Show error message when diagnostics failed
- fix enclosed_by_quotes
- fix a bug in returning NA locations
- requires collections 0.3.0
- Run tasks with delay to reduce CPU usage on input
- Refine args (Merge 9fd102b)
- respect NAMESPACE file


# languageserver 0.3.5

- Remove dependency on readr
- Use stringi to replace stringr
- Respect snippetSupport
- Respect linter_file in diagnostics


# languageserver 0.3.4

- on-type-formatting
- documentLinkProvider
- textDocument/documentColor
- use writeLines to write raw data in Windows
- Support section/subsection in document symbols
- make sure the output are UTF8
- set O_BINARY for stdin/stdout
- allows user to override ServerCapabilities
- Incremental resolving packages
- Disable lintr for temp files
- and a lot of minor fixes and improvements


# languageserver 0.3.3

- xml based parsing which provides more information on hovering, definitions and completions
- a bug fix in reading stdin
- unicode support improvement
- some internal refactoring

# languageserver 0.3.2

- read other header fields
- require newer version of collections
- allow colon to trigger completion
- specify kind for non exported package objects
- Only provide package exported objects at :: (#84)
- run tests on github actions
- implementation of scope completion using xpath
- Allow user customizable formatting style
- use readr's read_lines and write_lines
- use a better function name
- Provide completion for language constants
- bump lintr to 2.0.0


# languageserver 0.3.1

- Recursive parsing (#56)
- improve way to check if process becomes orphan
- unicode support


# languageserver 0.3.0

- added symbol definitions
- added document and workspace symbols

# languageserver 0.2.6

- fix a bug in completion items
- lower bound lintr to 1.0.3
- fix a bug in desc_get_deps
- better support vscode settings

# languageserver 0.2.5

- deprecate languageserver.default_linters

# languageserver 0.2.4

- require latest styler release
- handle windows crlf
- disable homedir config until lintr is released
- concat multiple lines in signature
- Allow package name to contain dots (e.g. data.table)
- completing variables defined in document
- support completions and function signatures from documents
- improve worksync consistency
- sync all R files of a package
- load packages in Depends field


# languageserver 0.2.3

- ensure unicode documentation
- use * as itemBullet for hover help
- check if rootUri is NULL
