#This is not code
#This is a standard page for HKOinfo parameter rules

#Basic requirements
#ETime = Ending Time of download duration
#DDays = Duration of Time between ETime and STime
#listfail = List all failed downloads

#Special requirements
#STime = Starting Time of download duration
#lan = language of descriptive text
#type = Type of data download



#Download data structure
#Main Directory
# /Data
# /SATL      #the abbr for data type
# /SATL_02dc #attributes of the data
# /2022      #Year
# /202208    #Year and Month
# /20220801  #Year and Month and Date
# /filename...

#File name
#SART_en_20220801_0000.csv
#SATL_02dc_20220801_0000.png

#Type of data
#export(load_gtmp_csv) = load grass temperature data from HK gov open data, fil3
#export(load_ltng)     = load lighting image from HKO website, fil2
#export(load_mslp_csv) = load mean sea level pressure from HK gov open data, fil3
#export(load_rain_csv) = load rainfall nowcast data from HK gov open data, fil3
#export(load_rain_dy)  = load rainfall daily data from HKO, fil2
#export(load_rain_hr)  = load rainfall hourly data from HKO, fil2
#export(load_rhum_csv) = load relative humidity data from HK gov open data, fil3
#export(load_sart_csv) = load solar radiation data from HK gov open data, fil3
#export(load_satl)     = load satellite image data from HKO, fil2
#export(load_temp_csv) = load surface air temperature from HK gov open data, fil3
#export(load_tide_csv) = load actual tide data from HK gov open data, fil3
#export(load_wind)     = load wind image from HKO, fil2
#export(load_wind_csv) = load wind data from HK gov open data, fil3
#export(load_wxph)     = load weather images from HKO, fil2


