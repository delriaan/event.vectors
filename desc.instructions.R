# Copy this file into the package project root; edit as desired before executing
library(stringi)
.this.desc <- desc::desc(); .this.desc$print();
.initialize <- !stringi::stri_extract_last_words(rstudioapi::getActiveProject()) %in% installed.packages()[, "Package"];
.current_ver <- packageVersion(stringi::stri_extract_last_regex(getwd(), pattern = "[a-zA-Z._]+"));

if (.initialize){
	.people = { list(
			Chionesu = person(
				given = "Chionesu"
				, family = "George"
				, email = "cgeorge@alliancehealthplan.org"
				, role = c("aut", "cre")
				)
			, Heather = person(
					given = "Heather"
					, family = "Copley"
					, email = "hcopley@alliancehealthplan.org"
					, role = c("aut", "cre")
					)
				, DScience = person(
					given = "Alliance Health Data Science Department"
					, email = "DScience@alliancehealthplan.org"
					)
			)}
	.this.desc$set("Author", .people$Chionesu)
	.this.desc$set("Maintainer", .people$DScience)
	.this.desc$set("License", "Unicorn Overlords")
	.this.desc$set("Description", paste(readLines("EVSpace_pkg_desc.txt"), collapse = " "))
	.this.desc$set("Title", "Event Vector Space: Analysis of event relations")
	.this.desc$set_version('0.0.1')
	.this.desc$write()
	.this.desc$print()
}

# Create Important Files
purrr::walk(c("validation.R", "ChangeLog", "NEWS"), ~if (!file.exists(.x)){ file.create(.x) })

# On-Demand Populate NEwS and ChangeLog
cat(c(readLines("NEWS"), sprintf("[%s]", Sys.time()),), sep = "\n", file = "NEWS")

desc::desc_bump_version(3, normalize = TRUE);
cat(
	sprintf("Version %s", desc::desc_get_version())
	, "- event.vector.space{graph.events()}: Added coded to create vertex names from titles"
	, "- New package function: Added evs_retrace()"
	, "- Changed validation messages to use `cat()` instead of `print()"
	, readLines("ChangeLog")
	, sep = "\n ----- \n"
	, file = "ChangeLog"
	)
