*! version 1.1.4, Ben Jann, 29nov2006
*! wrapper for estout

program define esta, rclass
	version 8.2

// mode specific defaults
// - fixed
	local fixed_open0         `""% `c(current_date)' `c(current_time)'""'
	local fixed_close0        `""""'
	local fixed_open          `""'
	local fixed_close         `""'
	local fixed_caption       `"@title"'
	local fixed_open2         `""'
	local fixed_close2        `""'
	local fixed_toprule       `"@hline"'
	local fixed_midrule       `"@hline"'
	local fixed_bottomrule    `"@hline"'
	local fixed_topgap        `""""'
	local fixed_midgap        `""""'
	local fixed_bottomgap     `""""'
	local fixed_ssl           `"N R-sq "adj. R-sq" "pseudo R-sq" AIC BIC"'
	local fixed_lsl           `"Observations R-squared "Adjusted R-squared" "Pseudo R-squared" AIC BIC"'
	local fixed_starlevels    `"* 0.05 ** 0.01 *** 0.001"'
	local fixed_begin         `""'
	local fixed_delimiter     `"" ""'
	local fixed_end           `""'
	local fixed_varwidth      `"\`= cond("\`label'"=="", 12, 20)'"'
	local fixed_modelwidth    `"12"'
	local fixed_abbrev        `"abbrev"'
	local fixed_substitute    `""'
// - tab
	local tab_open0           `"`macval(fixed_open0)'"'
	local tab_close0          `""""'
	local tab_open            `""'
	local tab_close           `""'
	local tab_caption         `"@title"'
	local tab_open2           `""'
	local tab_close2          `""'
	local tab_toprule         `""'
	local tab_midrule         `""'
	local tab_bottomrule      `""'
	local tab_topgap          `""""'
	local tab_midgap          `""""'
	local tab_bottomgap       `""""'
	local tab_ssl             `"`macval(fixed_ssl)'"'
	local tab_lsl             `"`macval(fixed_lsl)'"'
	local tab_starlevels      `"`macval(fixed_starlevels)'"'
	local tab_begin           `""'
	local tab_delimiter       `"_tab"'
	local tab_end             `""'
	local tab_varwidth        `""'
	local tab_modelwidth      `""'
	local tab_abbrev          `""'
	local tab_substitute      `""'
// - csv
	local csv_open0           `"`"\`csvlhs'% `c(current_date)' `c(current_time)'""'"'
	local csv_close0          `""""'
	local csv_open            `""'
	local csv_close           `""'
	local csv_caption         `"`"\`csvlhs'@title""'"'
	local csv_open2           `""'
	local csv_close2          `""'
	local csv_toprule         `""'
	local csv_midrule         `""'
	local csv_bottomrule      `""'
	local csv_topgap          `""""'
	local csv_midgap          `""""'
	local csv_bottomgap       `""""'
	local csv_ssl             `"`macval(fixed_ssl)'"'
	local csv_lsl             `"`macval(fixed_lsl)'"'
	local csv_starlevels      `"`macval(fixed_starlevels)'"'
	local csv_begin           `"`"\`csvlhs'"'"'
	local csv_delimiter       `"`"";\`csvlhs'"'"'
	local csv_end             `"`"""'"'
	local csv_varwidth        `""'
	local csv_modelwidth      `""'
	local csv_abbrev          `""'
	local csv_substitute      `""'
// - rtf
	local rtf_open0           `""'
	local rtf_close0          `""'
	local rtf_open            `"{\rtf1`=cond("`c(os)'"=="MacOSX", "\mac", "\ansi")'"'
	local rtf_close           `"}"'
	local rtf_caption         `"@title \par"'
	local rtf_open2           `""'
	local rtf_close2          `""'
	local rtf_toprule         `""'
	local rtf_midrule         `""'
	local rtf_bottomrule      `""'
	local rtf_topgap          `""""'
	local rtf_midgap          `""""'
	local rtf_bottomgap       `""""'
	local rtf_ssl             `"N "R{\super 2}" "adj. R{\super 2}" "pseudo R{\super 2}" AIC BIC"'
	local rtf_lsl             `"Observations "R{\super 2}" "Adjusted R{\super 2}" "Pseudo R{\super 2}" AIC BIC"'
	local rtf_starlevels      `""{\super *}" 0.05 "{\super **}" 0.01 "{\super **}" 0.001"'
	local rtf_begin           `""\trowd\intbl\trgaph108\trql\trleft-108\`rtf_twidth'\trautofit1\cellx\ql ""'
	      local rtf_twidth    `"\`=cond("\`width'"=="", "", "\trftsWidth2\trwWidth\`=50*\`rtf_twidthf''")'"'
	      local rtf_twidthf   `"\`=cond("\`width'"=="", "0", "\`width'")'"'
	local rtf_delimiter       `"`"\cell\cellx\q\`=cond(`"\`alignment'"'!="", `"\`alignment'"', "c")' "'"'
	local rtf_end             `"\cell\row"'
	local rtf_varwidth        `""'
	local rtf_modelwidth      `""'
	local rtf_abbrev          `""'
	local rtf_substitute      `""'
// - html
	local html_open0          `"<html> <head> "<title>`=cond(`"\`macval(title)'"'=="","estimates table, created `c(current_date)' `c(current_time)'","@title")'</title>" </head> <body> """'
	local html_close0         `""" </body> </html> """'
	local html_open           `"`"<table border="0" width="\`=cond("\`width'"=="","*","\`width'")'">"'"'
	local html_close          `"</table>"'
	local html_caption        `"<caption>@title</caption>"'
	local html_open2          `""'
	local html_close2         `""'
	local html_toprule        `""<tr><td colspan=@span><hr></td></tr>""'
	local html_midrule        `""<tr><td colspan=@span><hr></td></tr>""'
	local html_bottomrule     `""<tr><td colspan=@span><hr></td></tr>""'
	local html_topgap         `""'
	local html_midgap         `""<tr><td colspan=@span>&nbsp;</td></tr>""'
	local html_bottomgap      `""'
	local html_ssl            `"N R<sup>2</sup> "adj. R<sup>2</sup>" "pseudo R<sup>2</sup>" AIC BIC"'
	local html_lsl            `"Observations R<sup>2</sup> "Adjusted R<sup>2</sup>" "Pseudo R<sup>2</sup>" AIC BIC"'
	local html_starlevels     `"<sup>*</sup> 0.05 <sup>**</sup> 0.01 <sup>***</sup> 0.001"'
	local html_begin          `"<tr><td>"'
	local html_delimiter      `"</td><td\`=cond(`"\`alignment'"'!="", `" align="\`alignment'""', "")'>"'
	local html_end            `"</td></tr>"'
	local html_varwidth       `"\`= cond("\`label'"=="", 12, 20)'"'
	local html_modelwidth     `"12"'
	local html_abbrev         `""'
	local html_substitute     `""'
// - tex
	local tex_open0           `""% `c(current_date)' `c(current_time)'" \documentclass{article} \begin{document} """'
	local tex_close0          `""" \end{document} """'
	local tex_open            `"\begin{table}[htbp]\centering"'
	local tex_close           `"\end{table}"'
	local tex_caption         `"\caption{@title}"'
	local tex_open2           `"\`=cond("\`width'"=="", "\begin{tabular}", `"\begin{tabular*}{\`width'}"')'"'
	local tex_close2          `"\`=cond("\`width'"=="", "\end{tabular}", `"\end{tabular*}"')'"'
	local tex_toprule         `"\hline\hline"'
	local tex_midrule         `"\hline"'
	local tex_bottomrule      `"\hline\hline"'
	local tex_topgap          `""'
	local tex_midgap          `"[1em]"' // `"\\\"'
	local tex_bottomgap       `""'
	local tex_ssl             `"N R$^{2}$ "adj. R$^{2}$" "pseudo R$^{2}$>" AIC BIC"'
	local tex_lsl             `"Observations R$^{2}$ "Adjusted R$^{2}$" "Pseudo R$^{2}$" AIC BIC"'
	local tex_starlevels      `"\sym{*} 0.05 \sym{**} 0.01 \sym{***} 0.001"'
	local tex_begin           `""'
	local tex_delimiter       `"&"'
	local tex_end             `"\\\"'
	local tex_varwidth        `"\`= cond("\`label'"=="", 12, 20)'"'
	local tex_modelwidth      `"12"'
	local tex_abbrev          `""'
	local tex_substitute      `"_ \_ "\_cons " \_cons < $<$"'
// - booktabs
	local booktabs_open0      `""% `c(current_date)' `c(current_time)'" \documentclass{article} \usepackage{booktabs} \begin{document} """'
	local booktabs_close0     `"`macval(tex_close0)'"'
	local booktabs_open       `"`macval(tex_open)'"'
	local booktabs_close      `"`macval(tex_close)'"'
	local booktabs_caption    `"`macval(tex_caption)'"'
	local booktabs_open2      `"`macval(tex_open2)'"'
	local booktabs_close2     `"`macval(tex_close2)'"'
	local booktabs_toprule    `"\toprule"'
	local booktabs_midrule    `"\midrule"'
	local booktabs_bottomrule `"\bottomrule"'
	local booktabs_topgap     `"`macval(tex_topgap)'"'
	local booktabs_midgap     `"\addlinespace"'
	local booktabs_bottomgap  `"`macval(tex_bottomgap)'"'
	local booktabs_ssl        `"`macval(tex_ssl)'"'
	local booktabs_lsl        `"`macval(tex_lsl)'"'
	local booktabs_starlevels `"`macval(tex_starlevels)'"'
	local booktabs_begin      `"`macval(tex_begin)'"'
	local booktabs_delimiter  `"`macval(tex_delimiter)'"'
	local booktabs_end        `"`macval(tex_end)'"'
	local booktabs_varwidth   `"`macval(tex_varwidth)'"'
	local booktabs_modelwidth `"`macval(tex_modelwidth)'"'
	local booktabs_abbrev     `"`macval(tex_abbrev)'"'
	local booktabs_substitute `"`macval(tex_substitute)'"'

// hello estout
	capt findfile estout.ado
	if _rc {
		di as err "-estout- is required; type {stata ssc install estout}"
		error 499
	}

// syntax
	syntax [anything] [using] [ , ///
/// coefficients and t-stats, se, etc.
	 b Bfmt(string) ///
	 noT Tfmt(string) ///
	 se SEfmt(string) ///
	 p Pfmt(string) ///
	 ci CIfmt(string) ///
	 BEta BEtafmt(string) ///
	 main(string) /// syntax: name format
	 aux(string) /// syntax: name format
	 abs  /// absolute t-values
	 wide ///
	 NOSTAr STAR ///
	 staraux ///
	 NOCONstant CONstant ///
	 COEFlabels(string asis) ///
/// summary statistics
	 noOBS obslast ///
	 r2 R2fmt(string) ar2 AR2fmt(string) pr2 PR2fmt(string) ///
	 aic AICfmt(string) bic BICfmt(string) ///
	 SCAlars(string asis) /// syntax: "name1 [label1]" "name2 [label2]" etc.
	 sfmt(string) ///
/// layout
	 NODEPvars DEPvars ///
	 NOPArentheses PArentheses ///
	 BRackets ///
	 NONOTEs NOTEs /// without s in helpfile
	 NOMTItles MTItles MTItles2(string asis) ///
	 NOGAPs GAPs ///
	 NOLInes LInes ///
	 ADDNotes(string asis) ///
	 COMpress ///
	 plain ///
	 fixed tab csv rtf html tex BOOKTabs ///
	 Fragment ///
	 page ///
	 ALIGNment(str asis) ///
	 width(str asis) ///
/// other
	 Noisily ///
	 * ]
	_estout_options , `macval(options)'

// syntax consistency etc
	gettoken chunk using0: using
	foreach opt in constant gaps lines star abbrev ///
	 depvars numbers parentheses notes mtitles type {
		NotBothAllowed "``opt''" `no`opt''
	}
	NotBothAllowed "`staraux'" `nostar'
	if `"`macval(mtitles2)'"'!="" NotBothAllowed "mtitles" `nomtitles'
	if `"`pfmt'"'!=""    local p p
	if `"`sefmt'"'!=""   local se se
	if `"`cifmt'"'!=""   local ci ci
	if `"`betafmt'"'!="" local beta beta
	if "`level'"==""     local level $S_level
	if ((("`margin'"!="" | `"`margin2'"'!="") & "`nomargin'"=="") | ///
	   ("`beta'"!="") | ("`eform'"!="" & "`noeform'"=="")) ///
	   & "`constant'"==""  local noconstant noconstant
	if `"`r2fmt'"'!="" local r2 r2
	if `"`ar2fmt'"'!="" local ar2 ar2
	if `"`pr2fmt'"'!="" local pr2 pr2
	if `"`aicfmt'"'!="" local aic aic
	if `"`bicfmt'"'!="" local bic bic
	if "`type'"=="" & `"`using'"'!="" local notype notype

// format modes
	local mode `fixed' `tab' `csv' `rtf' `html' `tex' `booktabs'
	if `:list sizeof mode'>1 {
		di as err "only one allowed of fixed, tab, csv, rtf, html, tex, or booktabs"
		exit 198
	}
	if "`mode'"=="" {
		if `"`using'"'!="" {
			_getfilename `"`using0'"'
			_getfilesuffix `"`r(filename)'"'
			local suffix `"`r(suffix)'"'
			if inlist(`"`suffix'"', ".html", ".htm") local mode html
			else if `"`suffix'"'==".tex" local mode tex
			else if `"`suffix'"'==".csv" local mode csv
			else if `"`suffix'"'==".rtf" local mode rtf
			else local mode fixed
		}
		else local mode fixed
	}
	local mode0 `mode'
	if "`mode0'"=="booktabs" local mode0 tex
	else if "`mode0'"=="csv" {
		if "`plain'"=="" local csvlhs `"=""'
		else local csvlhs `"""'
	}
	if "`compress'"!="" & !inlist("`mode0'", "tab", "csv", "rtf") {
		if "`modelwidth'"=="" local modelwidth modelwidth(9)
		if "`varwidth'"=="" {
			if "`label'"!="" local varwidth varwidth(16)
			else             local varwidth varwidth(10)
		}
	}
	if "`plain'"=="" {
		foreach opt in star depvars numbers parentheses notes lines {
			SwitchOnIfEmpty `opt' `no`opt''
		}
		if "`wide'"=="" & ("`t'"=="" | "`se'`p'`ci'`aux'"!="") ///
		 SwitchOnIfEmpty gaps `nogaps'
	}
	if inlist("`mode0'", "tab", "csv", "rtf") local lines
	if inlist("`mode0'", "rtf") local gaps
	if "`notes'"!="" & "`nolegend'"=="" local legend legend
	if "`plain'"!="" {
		if "`bfmt'"==""    local bfmt %9.0g
		if "`tfmt'"==""    local tfmt `bfmt'
		if "`sefmt'"==""   local sefmt `bfmt'
		if "`pfmt'"==""    local pfmt `bfmt'
		if "`cifmt'"==""   local cifmt `bfmt'
		if "`betafmt'"=="" local betafmt `bfmt'
	}
	if "`nomtitles'"!="" local depvars
	else if "`depvars'"=="" local mtitles mtitles

// cells() option
	if "`notes'"!="" {
		if ("`margin'"!="" | `"`margin2'"'!="") & "`nomargin'"=="" ///
		 local note "`note'Marginal effects"
		if "`eform'"!="" & "`noeform'"=="" ///
		 local note "`note'Exponentiated coefficients"
	}
	if "`bfmt'"=="" local bfmt a3
	if `"`macval(cells)'"'=="" {
		if "`star'"!="" & "`staraux'"=="" local bstar star
		if "`beta'"!="" {
			if "`main'"!="" {
				di as err "beta() and main() not allowed both"
				exit 198
			}
			if "`betafmt'"==""  local betafmt 3
			local cells fmt(`betafmt') `bstar'
			local cells beta(`cells')
			if "`notes'"!="" {
				if `"`note'"'!="" local note "`note'; "
				local note "`note'Standardized beta coefficients"
			}
		}
		else if "`main'"!="" {
			tokenize "`main'"
			if "`2'"=="" local 2 "`bfmt'"
			local cells fmt(`2') `bstar'
			local cells `1'(`cells')
			if "`notes'"!="" {
				if `"`note'"'!="" local note "`note'; "
				local note "`note'`1' coefficients"
			}
		}
		else {
			local cells fmt(`bfmt') `bstar'
			local cells b(`cells')
		}
		if "`t'"=="" | "`se'`p'`ci'`aux'"!="" {
// parse aux optio
			tokenize "`aux'"
			local auxname `1'
			local auxfmt `2'
// type of auxiliary statistic
			local aux `se' `p' `ci' `auxname'
			if `"`aux'"'=="" local aux t
			else {
				if `:list sizeof aux'>1 {
					di as err "only one allowed of se, p, ci, and aux()"
					exit 198
				}
			}
			if `"`aux'"'!="t"  local abs
// parentheses/brackets
			if "`parentheses'"!="" | "`brackets'"!="" {
				if `"`aux'"'=="ci" {
					local brackets brackets
					local paren par
				}
				else if "`brackets'"!="" local paren "par([ ])"
				else local paren par
			}
// compose note
			if "`notes'"!="" {
				if `"`note'"'!="" local note "`note'; "
				if `"`auxname'"'!="" {
					local note `"`macval(note)'`auxname'"'
				}
				else if `"`aux'"'=="t"  {
					if "`abs'"!="" local note `"`macval(note)'Absolute "'
					local note `"`macval(note)'t-statistics"'
				}
				else if `"`aux'"'=="se" {
					local note `"`macval(note)'Standard errors"'
				}
				else if `"`aux'"'=="p" {
					local note `"`macval(note)'p-values"'
				}
				else if `"`aux'"'=="ci" {
					local note `"`macval(note)'`level'% confidence intervalls"'
				}
				if "`parentheses'"=="" {
					if "`wide'"=="" local note `"`macval(note)' in second row"'
					else local note `"`macval(note)' in second column"'
				}
				else if "`brackets'"!="" {
					local note `"`macval(note)' in brackets"'
				}
				else local note `"`macval(note)' in parentheses"'
			}
// formats
			if "`tfmt'"==""     local tfmt 2
			if "`sefmt'"==""    local sefmt `bfmt'
			if "`pfmt'"==""     local pfmt 3
			if "`cifmt'"==""    local cifmt `bfmt'
			if `"`auxfmt'"'=="" local auxfmt `bfmt'
			if `"`auxname'"'=="" {
				local auxfmt ``aux'fmt'
			}
// stars
			if "`staraux'"!="" local staraux star
// put together
			local bin fmt(`auxfmt') `paren' `abs' `staraux'
			local cells `cells' `aux'(`bin')
		}
		if "`wide'"!="" local cells cells(`"`cells'"')
		else            local cells cells(`cells')
	}

// stats() option
	if `"`macval(stats)'"'=="" {
		if `"`sfmt'"'=="" local sfmt `bfmt'
		if `"`r2fmt'"'=="" local r2fmt = cond("`plain'"!="", "`bfmt'", "3")
		if `"`ar2fmt'"'=="" local ar2fmt = cond("`plain'"!="", "`bfmt'", "3")
		if `"`pr2fmt'"'=="" local pr2fmt = cond("`plain'"!="", "`bfmt'", "3")
		if `"`aicfmt'"'=="" local aicfmt `bfmt'
		if `"`bicfmt'"'=="" local bicfmt `bfmt'
		if "`label'"=="" {
			local stalabs `"``mode'_ssl'"'
		}
		else {
			local stalabs `"``mode'_lsl'"'
		}
		gettoken obslab stalabs: stalabs
		if "`obs'"=="" & "`obslast'"=="" {
			local sta N
			local stalab `"`macval(obslab)'"'
			local stafmt %18.0g
		}
		local i 0
		foreach s in r2 ar2 pr2 aic bic {
			local ++i
			if "``s''"!="" {
				local sta `sta' `:word `i' of r2 r2_a r2_p aic bic'
				local chunk: word `i' of `macval(stalabs)'
				local stalab `"`macval(stalab)' `"`macval(chunk)'"'"'
				local stafmt `stafmt' ``s'fmt'
			}
		}
		local i 0
		foreach addstat of local scalars {
			local ++i
			gettoken addstatname addstatlabel: addstat
			if `: list posof `"`addstatname'"' in sta' continue
			if `"`addstatname'"'=="N" & "`obs'"=="" & "`obslast'"!="" continue
			if trim(`"`macval(addstatlabel)'"')=="" local addstatlabel `addstatname'
			local addstatfmt: word `i' of `sfmt'
			if `"`addstatfmt'"'=="" {
				local addstatfmt: word `: list sizeof sfmt' of `sfmt'
			}
			local sta `sta' `addstatname'
			local stalab `"`macval(stalab)' `"`macval(addstatlabel)'"'"'
			local stafmt `stafmt' `addstatfmt'
		}
		if "`obs'"=="" & "`obslast'"!="" {
			local sta `sta' N
			local stalab `"`macval(stalab)' `macval(obslab)'"'
			local stafmt `stafmt' %18.0g
		}
		if "`sta'"!="" {
			local stats stats(`sta', fmt(`stafmt') labels(`macval(stalab)'))
		}
	}

// table header
	if `"`macval(mlabels)'"'=="" {
		if "`mode0'"=="tex" local mspan " span prefix(\multicolumn{@span}{c}{) suffix(})"
		if `"`depvars'"'!="" {
			local mlabels `"mlabels(, depvar`mspan')"'
		}
		else local mlabels `"mlabels(, none)"'
		if "`mtitles'"!="" {
			if "`mode0'"=="tex" local mlabels `"mlabels(,`mspan')"'
			else local mlabels
		}
		if `"`macval(mtitles2)'"'!="" {
			local mlabels `"mlabels(`macval(mtitles2)',`mspan')"'
		}
	}
	if `"`macval(collabels)'"'=="" {
		local collabels `"collabels(, none)"'
	}
	if "`mode0'"=="tex" & "`numbers'"!="" {
		local numbers "numbers(\multicolumn{@span}{c}{( )})"
	}

// pre-/posthead, pre-/postfoot, gaps and lines
// - complete note
	if `"`macval(note)'"'!="" {
		local note `"`"`macval(note)'"'"'
	}
	if "`legend'"!="" {
		if ("`margin'"!="" | `"`margin2'"'!="") & ///
		   "`nomargin'"=="" & "`nodiscrete'"=="" {
			local note `"`macval(note)' @discrete"'
		}
		if "`star'"!="" {
			local note `"`macval(note)' @starlegend"'
		}
	}
	if `"`macval(addnotes)'"'!="" {
		local note `"`macval(note)' `macval(addnotes)'"'
	}
// - mode specific settings
	foreach opt in starlevels begin delimiter end ///
	 varwidth modelwidth substitute {
		if `"`macval(`opt')'"'=="" & `"`macval(`mode'_`opt')'"'!="" {
			local `opt' `"`opt'(``mode'_`opt'')"'
		}
	}
	if "`noabbrev'`abbrev'"=="" {
		local abbrev ``mode'_abbrev'
	}
	if `"`fragment'"'=="" {
		if "`page'"!="" {
			local opening `"``mode'_open0'"'
		}
		if `"`macval(title)'"'!="" {
			local opening `"`macval(opening)' ``mode'_open'"'
			if "`mode0'"=="tex" & "`star'"!="" {
				local opening `"`macval(opening)' \def\sym#1{\ifmmode^{#1}\else$^{#1}$\fi}"'
			}
			local opening `"`macval(opening)' ``mode'_caption'"'
		}
		else if "`mode0'"=="tex" & "`star'"!="" {
			local opening `"`macval(opening)' { \def\sym#1{\ifmmode^{#1}\else$^{#1}$\fi}"'
		}
		else if "`mode0'"!="tex" {
			local opening `"`macval(opening)' ``mode'_open'"'
		}
		local opening `"`macval(opening)' ``mode'_open2'"'
		if  "`mode0'"=="tex" {
			if `"`width'"'!="" local extracolsep "@{\hskip\tabcolsep\extracolsep\fill}"
			if `"`macval(alignment)'"'!="" {
				local opening `"`macval(opening)'{`extracolsep'l*{@E}{`macval(alignment)'}}"'
			}
			else {
				MakeTeXColspec "`wide'" "`not'" "`star'" "`stardetach'" "`staraux'"
				local opening `"`macval(opening)'{`extracolsep'l*{@E}{`value'}}"'
			}
		}
		if "`mode0'"=="html" {
			local brr
			foreach chunk of local note {
				local closing `"`macval(closing)' `"`brr'`macval(chunk)'"'"'
				local brr "<br />"
			}
			if `"`macval(closing)'"'!="" {
				local closing `""<tr><td colspan=@span>" `macval(closing)' </td></tr>"'
			}
		}
		else if "`mode0'"=="tex" {
			foreach chunk of local note {
				local closing `"`macval(closing)' `"\multicolumn{@span}{l}{\footnotesize `macval(chunk)'}\\\"'"'
			}
		}
		else if "`mode0'"=="csv" {
			foreach chunk of local note {
				local closing `"`macval(closing)' `"`csvlhs'`macval(chunk)'""'"'
			}
		}
		else if "`mode0'"=="rtf" {
			if `"`macval(note)'"'!="" local closing "\pard"
			foreach chunk of local note {
				local closing `"`macval(closing)' `"`macval(chunk)'\par"'"'
			}
		}
		else {
			local closing `"`macval(note)'"'
		}
		local closing `"`macval(closing)' ``mode'_close2'"'
		if `"`macval(title)'"'!="" | "`mode0'"!="tex" {
			local closing `"`macval(closing)' ``mode'_close'"'
		}
		else if "`mode0'"=="tex" & "`star'"!="" {
			local closing `"`macval(closing)' }"'
		}
		if "`page'"!="" {
			local closing `"`macval(closing)' ``mode'_close0'"'
		}
		local toprule    `"``mode'_toprule'"'
		local bottomrule `"``mode'_bottomrule'"'
		local topgap     `"``mode'_topgap'"'
		local bottomgap  `"``mode'_bottomgap'"'
	}
	local midrule `"``mode'_midrule'"'
	local midgap  `"``mode'_midgap'"'
// - compose prehead()
	if `"`macval(prehead)'"'=="" {
		if `"`lines'"'!="" {
			local opening `"`macval(opening)' `macval(toprule)'"'
		}
		else if `"`gaps'"'!="" {
			local opening `"`macval(opening)' `macval(topgap)'"'
		}
		SaveRetok `macval(opening)'
		local opening `"`macval(value)'"'
		if `"`macval(opening)'"'!="" {
			local prehead `"prehead(`macval(opening)')"'
		}
	}
// - compose posthead()
	if `"`macval(posthead)'"'=="" {
		if `"`lines'"'!="" {
			local posthead `"posthead(`macval(midrule)')"'
		}
		else if `"`gaps'"'!="" {
			local posthead `"posthead(`macval(midgap)')"'
		}
	}
// - compose prefoot()
	if `"`macval(prefoot)'"'=="" & `"`macval(stats)'"'!="" {
		if `"`lines'"'!="" {
			local prefoot `"prefoot(`macval(midrule)')"'
		}
		else if `"`gaps'"'!="" {
			local prefoot `"prefoot(`macval(midgap)')"'
		}
	}
// - compose postfoot()
	if `"`macval(postfoot)'"'=="" {
		if `"`lines'"'!="" {
			local closing `"`macval(bottomrule)' `macval(closing)'"'
		}
		else if `"`gaps'"'!="" {
			local closing `"`macval(bottomgap)' `macval(closing)'"'
		}
		SaveRetok `macval(closing)'
		local closing `"`macval(value)'"'
		if `"`macval(closing)'"'!="" {
			local postfoot postfoot(`macval(closing)')
		}
	}

	if `"`macval(varlabels)'"'=="" {
		if `"`gaps'"'!="" {
			local varl `", end("" `macval(midgap)') nolast"'
		}
		if "`label'"!=""  {
			local varl `"_cons Constant`macval(varl)'"'
		}
		if `"`macval(coeflabels)'"'!="" {
			local varl `"`macval(coeflabels)' `macval(varl)'"'
		}
		if trim(`"`macval(varl)'"')!="" {
			local varlabels varlabels(`macval(varl)')
		}
	}

// noconstant option
	if `"`drop'"'=="" {
		if "`noconstant'"!="" {
			local drop drop(_cons)
		}
	}

// compute beta coefficients (run estadd to add e(beta))
	if "`beta'"!="" {
		capt findfile estadd.ado
		if _rc {
			di as err "-estadd- is required to compute beta coefficients"
			di as err "type {stata ssc install estadd}"
			error 499
		}
		local estnames `"`anything'"'
		if `"`estnames'"'=="" {
			capt est_expand $esto
			if !_rc {
				local estnames `"$esto"'
			}
		}
		estadd beta, replace: `estnames'
	}

// execute estout
	CleanEstoutCmd `anything' `using' ,  ///
	 `macval(cells)' `drop' `nomargin' `margin' `margin2' `noeform' `eform'       ///
	 `nodiscrete' `macval(stats)' `stardetach' `macval(starlevels)'               ///
	 `varwidth' `modelwidth' `noabbrev' `abbrev' `unstack'                        ///
	 `macval(begin)' `macval(delimiter)' `macval(end)'                            ///
	 `macval(title)' `macval(prehead)' `macval(posthead)' `macval(prefoot)'       ///
	 `macval(postfoot)' `label' `macval(varlabels)' `macval(mlabels)' `nonumbers' ///
	 `numbers' `macval(collabels)' `macval(eqlabels)' `macval(mgroups)'           ///
	 `macval(substitute)' `notype'`type' level(`level') `macval(options)'

	if "`noisily'"!="" {
		gettoken chunk rest: cmd, parse(",")
		di as txt `"`chunk'"' _c
		gettoken chunk rest: rest, bind
		while `"`macval(chunk)'"'!="" {
			di as txt `" `macval(chunk)'"'
			gettoken chunk rest: rest, bind
		}
	}
	`macval(cmd)'
	if `"`using'"'!="" {
		di as txt `"(output written to {browse `using0'})"'
	}
	return local estout `macval(cmd)'
end

program _estout_options
	syntax [, ///
	 Cells(passthru) ///
	 Drop(passthru)  ///
///	 Keep(string asis) ///
///	 Order(string asis) ///
///	 Indicate(string asis) ///
///	 TRansform(string asis) ///
///	 EQuations(passthru) ///
	 NOEFORM eform ///EFORM2(string) ///
	 NOMargin Margin Margin2(passthru) ///
	 NODIscrete /// DIscrete(string asis) ///
///	 MEQs(string) ///
	 level(numlist max=1 int >=10 <=99) ///
	 Stats(passthru) ///
	 STARLevels(passthru) ///
///	 NOSTARDetach ///
	 STARDetach ///
	 VARwidth(passthru) ///
	 MODELwidth(passthru) ///
	 NOABbrev ABbrev ///
///	 NOUNStack
	 UNStack ///
	 BEGin(passthru) ///
	 DELimiter(passthru) ///
	 end(passthru) ///
///	 DMarker(string) ///
///	 MSign(string) ///
///	 NOLZ lz ///
	 SUBstitute(passthru) ///
	 TItle(passthru) ///
	 NOLEgend LEgend ///
	 PREHead(passthru) ///
	 POSTHead(passthru) ///
	 PREFoot(passthru) ///
	 POSTFoot(passthru) ///
///	 HLinechar(string) ///
///	 NOLabel
	 Label ///
	 VARLabels(passthru) ///
///	 REFcat(string asis) ///
	 MLabels(passthru) ///
	 NONUMbers NUMbers ///NUMbers2(string asis) ///
	 COLLabels(passthru) ///
	 EQLabels(passthru) ///
	 MGRoups(passthru) ///
///	 NOReplace Replace ///
///	 NOAppend Append ///
	 NOTYpe TYpe ///
///	 NOSHOWTABS showtabs ///
///	 STYle(string) ///
///	 DEFaults(string) ///
	 * ]
	foreach opt in ///
	 cells drop noeform eform nomargin margin margin2 nodiscrete ///
	 level stats starlevels stardetach varwidth modelwidth unstack ///
	 noabbrev abbrev begin delimiter end substitute title nolegend ///
	 legend prehead posthead prefoot postfoot label varlabels mlabels ///
	 nonumbers numbers collabels eqlabels mgroups notype type ///
	 options {
		c_local `opt' `macval(`opt')'
	}
end

prog NotBothAllowed
	args opt1 opt2
	if `"`opt1'"'!="" {
		if `"`opt2'"'!="" {
			di as err `"options `opt1' and `opt2' not both allowed"'
			exit 198
		}
	}
end

prog SwitchOnIfEmpty
	args opt1 opt2
	if `"`opt2'"'=="" {
		c_local `opt1' `opt1'
	}
end

prog _getfilesuffix, rclass // based on official _getfilename.ado
	version 8
	gettoken filename rest : 0
	if `"`rest'"' != "" {
		exit 198
	}
	local hassuffix 0
	gettoken word rest : filename, parse(".")
	while `"`rest'"' != "" {
		local hassuffix 1
		gettoken word rest : rest, parse(".")
	}
	if `"`word'"'=="." {
		di as err `"incomplete filename; ends in ."'
		exit 198
	}
	if `hassuffix' return local suffix `".`word'"'
	else           return local suffix ""
end

prog MakeTeXColspec
	args wide not star detach aux
	if "`star'"!="" & "`detach'"!="" & "`aux'"=="" local value "r@{}l"
	else local value "c"
	if "`wide'"!="" & "`not'"=="" {
		if "`star'"!="" & "`detach'"!="" & "`aux'"!="" local value "`value'r@{}l"
		else local value "`value'c"
	}
	c_local value "`value'"
end

prog SaveRetok
	gettoken chunk 0: 0, q
	local value `"`macval(chunk)'"'
	gettoken chunk 0: 0, q
	while `"`macval(chunk)'"'!="" {
		local value `"`macval(value)' `macval(chunk)'"'
		gettoken chunk 0: 0, q
	}
	c_local value `"`macval(value)'"'
end

prog CleanEstoutCmd
	syntax [anything] [using] [ , * ]
	local cmd estout
	if `"`macval(anything)'"'!="" {
		local cmd `"`macval(cmd)' `macval(anything)'"'
	}
	if `"`macval(using)'"'!="" {
		local cmd `"`macval(cmd)' `macval(using)'"'
	}
	if `"`macval(options)'"'!="" {
		local cmd `"`macval(cmd)', `macval(options)'"'
	}
	c_local cmd `"`macval(cmd)'"'
end
