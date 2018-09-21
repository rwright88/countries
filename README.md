# countries

Country datasets

## Datasets

### Structure

One row per country per year, with these columns:

- country_code
- country_name
- year
- statistics (any number of columns)

### Completed

Dataset      | Source
-------------|-------
demographics | [UN](https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_TotalPopulationBySex.csv)
income       | [Conference Board](https://www.conference-board.org/retrievefile.cfm?filename=TED_FLATFILE_ORI_MAR20181.txt&type=subsite)
migration    | [UN](https://www.un.org/en/development/desa/population/migration/data/estimates2/estimates17.shtml)
companies    | [Forbes](https://www.forbes.com/global2000/list/)

### To do

Create better crosswalk with:

https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv

Add datasets:

- employment
- life expectancy
