# Search path
VPATH = data data-raw eda scripts reports

# Processed data files
DATA = member_averages.rds population.rds protests.rds protests_population.rds votes.rds

# EDA studies
EDA = protests.md protests_population.md member_averages_votes.md protests_population_member_averages_votes.md

# Reports
REPORTS =

# All targets
all : $(DATA) $(EDA) $(REPORTS)

# Data dependencies
protests_population.rds : protests.rds population.rds

# EDA study and report dependencies
protests_population.md : protests_population.rds

member_averages_votes.md : member_averages.rds votes.rds

protests_population_member_averages_votes.md : protests_population.rds member_averages.rds votes.rds


# Pattern rules
%.rds : %.R
	Rscript $<
%.md : %.Rmd
	Rscript -e 'rmarkdown::render(input = "$<", output_options = list(html_preview = FALSE))'
