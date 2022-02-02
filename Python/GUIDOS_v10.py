# PURPOSE: This script analyzes the type of fragmentation present in the specified
# land cover types (i.e. forest).
# The analysis in this script is equivalent to the procedure developed by Vogt et al. (2007) -
# testing has confirmed that this script generates identical results. The procedure used by
# Vogt et al. (2007) can be found in the following paper:
#
#   Vogt, P., K. Riitters, C. Estrenguil, J. Kozak, T. Wade, J. Wickham. 2007. Mapping spatial patterns
#       with morphological image processing. Landscape Ecology 22: 171-177
#
# Note: the analysis has been modified from Vogt et al. (2007) to sub-classify core patches
# into three categories based on area.
#-------------------------------------------------
# DESCRIPTION OF OUTPUT...
# The script will generate a raster that contains the following values:
#   0: the fragmenting class (i.e. non-forest)
#   1: patch
#   2: edge
#   3: perforated  
#   4: core < 100 hectares
#   5: core >= 100 ha and < 200 ha
#   6: core >= 200 ha
#---------------------------------------------------
# Original script, Jeffrey Evans
# Modified by Jim Oakleaf, TNC Nov 2012 to be used in ArcGIS 10
# import required modules...
import arcpy
from arcpy import env
from arcpy.sa import *
import os
import time
import traceback
import sys
import tempfile

def returnLinUnits(linUnits,outUnit):
    dist_units = linUnits.split()
    dist = dist_units[0]
    units = dist_units[1]
    if units == outUnit:
        return dist
    else:
        if outUnit == "Feet":
            if units == "Miles":
                cvFact = 5280
            elif units == "Meters":
                cvFact = 3.28084
            else:
                cvFact = 3280.84
            returnVal = float(dist) * cvFact
        else:
            if units == "Miles":
                cvFact = 1609.34
            elif units == "Feet":
                cvFact = 0.3048
            else:
                cvFact = 1000
            returnVal = float(dist) * cvFact
        return returnVal



class LicenseError(Exception):
    pass

class SpatialRefProjError (Exception):
    pass



try:
    
	#Check for spatial analyst license
    if arcpy.CheckExtension("Spatial") == "Available":
        arcpy.CheckOutExtension("Spatial")
    else:
        raise LicenseError

   # Set overwrite option
    env.overwriteOutput = True
 		
    # script parameters...
    landcover = arcpy.GetParameterAsText(0)          # input raster dataset
    edgeWidthUnits = arcpy.GetParameterAsText(1)          # string
    FragRasterOut = arcpy.GetParameterAsText(2)               # output raster location
    #FragMap_lyr = arcpy.GetParameterAsText(3)        # display layer
        
    s = time.clock()
    
    #arcpy.AddMessage(os.path.dirname(FragRasterOut))
    
    #Needs a current workspace in order to save files    
    #if env.workspace is None:
        #envTmp = arcpy.GetSystemEnvironment("TEMP")
        #arcpy.AddMessage(envTmp)
    #    env.workspace = os.path.dirname(FragRasterOut)
   # if env.scratchWorkspace is None:
   #     env.scratchWorkspace = os.path.dirname(FragRasterOut)
    #arcpy.AddMessage(env.scratchWorkspace)
    
    
    # provide user feedback...
   # arcpy.AddMessage("\nOUTPUT DATASET NAME IS:")
   # print "\nOUTPUT DATASET NAME IS:"
   # arcpy.AddMessage("\n\t%s\n" % FragMap)
   # print "\n\t%s\n" % FragMap
    
    #-----------------------------------------------------------
    # DETERMINE PROJECTION UNITS...
    
    desc = arcpy.Describe(landcover)    # describe object
    cellsize = desc.MeanCellHeight      # cell size
    spatRef = desc.SpatialReference
    units = spatRef.LinearUnitName      # projection units
    
    # Note: output units are the same as the input units
    
    # Note: area thresholds based on hectares (will need to convert units to meters)
    #arcpy.AddMessage("Units")
    if "Foot" in units:
        cf = .3048      # correction factor to convert feet to meters
        outUnit = "Feet"
    elif "Meter" in units:
        cf = 1          # no correction needed
        outUnit = "Meters"
    # if units are not in feet or meters, full analysis cannot be completed - exit script...
    else:
        print "Land cover has unknown units. Data may be unprojected or projection may be undefined."
        arcpy.AddError("Land cover has unknown units. Data may be unprojected or projection may be undefined.")
        arcpy.sys.exit(1)     # exit script with failure
    del desc
    
    #-----------------------------------------------------------
    # VALIDATE EDGE WIDTH PARAMETER...
    # edge width parameter must be greater than or equal to cellsize...
    #arcpy.AddMessage(cellsize)

    edgeWidth = returnLinUnits(edgeWidthUnits,outUnit)
    
    if edgeWidth < cellsize:
        print "\nEDGE WIDTH MUST BE GREATER THAN OR EQUAL TO PIXEL WIDTH\n"
        arcpy.AddError("\nEDGE WIDTH MUST BE GREATER THAN OR EQUAL TO PIXEL WIDTH\n")    
        arcpy.sys.exit(1)
                       
                       

    
    #-----------------------------------------------------------
    # SET UP TEMPORARY FOLDER FOR TEMPORARY DATA...
    
    # temporary workspace location...
    userTmp = tempfile.gettempdir()
    TempWS = userTmp+"\\guidos_tmp"

    
    # check if temp workspace exists...
    if not arcpy.Exists(TempWS):
        # create folder if it doesn't exist...
        os.makedirs(TempWS) 
    env.workspace = TempWS
    env.scratchWorkspace = TempWS
    env.overwriteOutput = True
    #---------------------------------------------------
    # EXTRACT DATA FROM LAND COVER RASTER...
    
    # extract fragmented class from land cover...
    f1_ras = Reclassify(landcover, "Value", "2 1", "NODATA")
    f1_ras.save("f1_ras.img")
	
    # extract fragmenting class from land cover...
    nf1_ras = Reclassify(landcover, "Value", "1 1", "NODATA")
    nf1_ras.save("nf1_ras.img")
    
    arcpy.AddMessage("DATA EXTRACTED FROM LAND COVER")
    #print "DATA EXTRACTED FROM LAND COVER"
    #---------------------------------------------------
    # EXTRACT EDGE CLASS FOR FRAGMENTED LAND COVER...
    
    # buffer fragmented class by width of edge zone...
    #arcpy.AddMessage("EucDistance")
    f_buf = EucDistance(f1_ras, edgeWidth, cellsize)
    f_buf.save("f_buf.img")
    # extract fragmenting class periphery (1) and core (2)...
    remap = "0 NoData;0 %s 1;NoData 2" % edgeWidth
    

    
    nf_peri_core = Reclassify(f_buf, "Value", remap, "data")
    nf_peri_core.save("nf_peri_core.img")
    # region group fragmenting class...
    nf_grp = RegionGroup(nf1_ras, "EIGHT", "WITHIN")
    nf_grp.save("nf_grp.img")
    # identify fragmenting class groups as core or periphery.
    nf_core_peri_grps = ZonalStatistics(nf_grp, "VALUE", nf_peri_core, "MAXIMUM", "DATA")
    nf_core_peri_grps.save("nf_core_peri_grps.img")
    # extract fragmenting class core groups...
    nf_core = Reclassify(nf_core_peri_grps, "Value", "2 1", "NODATA")
    nf_core.save("nf_core.img")
    # buffer fragementing class core groups by width of edge zone...
    nf_core_buf = EucDistance(nf_core, edgeWidth, cellsize, "")
    nf_core_buf.save("nf_core_buf.img")
    
    # extract edge forest (1)...
    remap = "0 NODATA;0 %s 1;NODATA 0" % edgeWidth
    edge = Reclassify(nf_core_buf, "Value", remap, "DATA")
    edge.save("edge.img")

    arcpy.AddMessage("EDGE FRAGMENTATION IDENTIFIED")
   # print "EDGE FRAGMENTATION IDENTIFIED"
    
    #---------------------------------------------------
    # EXTRACT PATCH, CORE, AND PERIPHERY OF FRAGMENTED CLASS...
    
    # buffer fragmenting class by width of edge zone...
    nf_buf = EucDistance(nf1_ras, edgeWidth, cellsize,"")
    nf_buf.save("nf_buf.img")
    # extract core and periphery of fragmented class...
    remap = "0 NoData;0 %s 2;NoData 4" % edgeWidth
    f_peri_core = Reclassify(nf_buf, "Value", remap, "NODATA")  
    f_peri_core.save("f_peri_core.img")
	
    # region group fragmented class...
    f_grp = RegionGroup(f1_ras, "FOUR", "WITHIN")
    f_grp.save("f_grp.img")
    # identify fragmented class groups as core or periphery...
    f_core_peri_grps = ZonalStatistics(f_grp, "VALUE", f_peri_core, "MAXIMUM", "DATA")
    f_core_peri_grps.save("f_core_peri_grps.img")
    # extract patch forest (0)...
    remap = "2 0;4 1;NoData 1"
    patch0_1 = Reclassify(f_core_peri_grps, "Value", remap, "NODATA") 
    patch0_1.save("patch0_1.img")
    arcpy.AddMessage("INTERIOR, PATCH, AND PERFORATED FRAGMENTATION IDENTIFIED")
    #print "INTERIOR, PATCH, AND PERFORATED FRAGMENTATION IDENTIFIED"
    #---------------------------------------------------
    # COMBINE FRAGMENTATION CLASSES AND CREATE FINAL FRAGMENTATION MAP...
    # Replace a layer/table view name with a path to a dataset (which can be a layer file) or create the layer/table view within the script
	# The following inputs are layers or table views: "f_peri_core", "edge"

    # combine fragmented class edge, core, and perforated...
    #nonPatch = f_peri_core + edge
    #Needed to start saving due to ArcGIS not handling variables for rasters at this point.
    #f_peri_core.save("fpericore")
    #edge.save("frag_edge")
    
    nonPatch = Plus("f_peri_core.img","edge.img")
    #nonPatch.save("c:\\zTemp\\nonpatch.img")
    # combine non-patch classes with patch - for fragemented class...
    nonPatch.save("fragnPatch.img")

    fragMap1 = Times("fragnPatch.img","patch0_1.img")
    fragMap1.save("fragMap1.img")
    # reclassify patch forest to 1...
    fragMap2 = Reclassify("fragMap1.img", "Value", "0 1", "DATA") 
    fragMap2.save("fragMap2.img")
    # clip fragMap to fragmented class pixels only...
    fragMap2.save("fragMap2.img")
    fragMap2_f = Times("fragMap2.img","f1_ras.img")

    #-------------------------------------------------------------------
    
    # DETERMINE PROJECTION UNITS...
    #JRO -- Setting again but now CF is setup for feet and eventually acres  
    
    #desc = arcpy.Describe(landcover)    # describe object
    #cellsize = desc.MeanCellHeight   # cell size
    #spatRef = desc.SpatialReference
    #units = spatRef.LinearUnitName    # projection units
    
    #JRO -- CF needs to be reset due to how calculations work   
    if "Foot" in units:
        cf = 1              # no correction needed
         
    elif "Meter" in units:
        cf = 3.2808          # correction factor to convert meters to feet
    
    # if units are not in feet or meters, full analysis cannot be completed - exit script...
    else:
        print "Land cover has unknown units. Data may be unprojected or projection may be undefined."
        arcpy.AddMessage("Land cover has unknown units. Data may be unprojected or projection may be undefined.")
        sys.exit(1)     # exit script with failure
    
    #del desc
    
    
    arcpy.AddMessage("CATEGORIZING CORE FOREST PATCHES...")
    #print "CATEGORIZING CORE FOREST PATCHES..."
    
    core_1 = Reclassify(fragMap2_f, "Value", "4 1", "NODATA")
    core_1.save("core.img")
    # region group core class...
    core_grp = RegionGroup(core_1, "EIGHT", "WITHIN")
    core_grp.save("core_grp.img")
    #core_grp.save("core_grp")
    # convert core groups to txt file...
    coreGrpFile = "%s\\coreGrpFile.txt" % TempWS
    arcpy.RasterToASCII_conversion("core_grp.img", coreGrpFile)
    
    # convert 4 class frag map to txt file...
    fragMap_4c = "%s\\fragMap_4c.txt" % TempWS
    arcpy.RasterToASCII_conversion(fragMap2_f, fragMap_4c)

    cur = arcpy.SearchCursor(core_grp)
    row = cur.next()
    
    remap_dct = {}
    
    # for each group...

   
    while row:

            grpID = row.getValue("VALUE")     # group ID
            count = row.getValue("COUNT")   # pixel count
            #Jro -- Area actually calculated to arces - 500 ac roughly 200 hectares
            # area in hectares...
            area = count * (cellsize*cf)**2 / 43560    
        
            # large core...
            if area >= 500:
                remap_dct[grpID] = 6
            # medium core...
            elif 250 <= area < 500:
                remap_dct[grpID] = 5
            # small core...
            else:
                remap_dct[grpID] = 4
        
            row = cur.next()

    
    if cur:
            del cur
    if row:
            del row
    fragMap_6c = "%s\\fragMap_6c.txt" % TempWS
    # input grid txt file...
    o_coreGrp = file(coreGrpFile,"r")
    o_fragMap_4c = file(fragMap_4c,"r")       # open file
    o_fragMap_6c = file(fragMap_6c,"w")
    # write header lines to output file...
    for x in range(6):
        o_fragMap_6c.write(o_coreGrp.readline())
        o_fragMap_4c.readline()
    
    # reset input grid to start...
    o_coreGrp.seek(0)
    
    # get raster properties...
    ncols = int(o_coreGrp.readline().split(" ")[-1])
    nrows = int(o_coreGrp.readline().split(" ")[-1])
    Xmin = float(o_coreGrp.readline().split(" ")[-1])
    Ymin = float(o_coreGrp.readline().split(" ")[-1])
    cellsize = float(o_coreGrp.readline().split(" ")[-1])
    NoData = o_coreGrp.readline().split(" ")[-1][:-1]
    for rowNo in xrange(nrows):
    
        grp_line = o_coreGrp.readline().split(" ")[:-1]
        frag_4c_line = o_fragMap_4c.readline().split(" ")[:-1]
        frag_6c_line = ""
        
        for colNo in xrange(ncols):
    
            grpID = int(grp_line[colNo])
            clss_4c = frag_4c_line[colNo]
    
            # swap perforated for edge (to be consistent with non-SA version)...
            if clss_4c == '2':
                frag_6c_line += "3 "
            # swap edge for perforated (to be consistent with non-SA version)...
            elif clss_4c == '3':
                frag_6c_line += "2 "
            # add in core tract size class...
            elif clss_4c == '4':
                frag_6c_line += "%s " % remap_dct[grpID]
            else:
                frag_6c_line += "%s " % clss_4c
    
        frag_6c_line += "\n"
        o_fragMap_6c.write(frag_6c_line)

    o_coreGrp.close()
    o_fragMap_4c.close()
    o_fragMap_6c.close()
    
            

    arcpy.ASCIIToRaster_conversion(fragMap_6c, FragRasterOut, "INTEGER") 
    arcpy.DefineProjection_management(FragRasterOut, spatRef) 

    # Define symbology 
    script_path=sys.argv[0]
    symb_dir = os.path.dirname(script_path) + '\\lyrfiles'
    frag_sym = symb_dir + '\\FragmentationMapLegend.lyr'
    params = arcpy.GetParameterInfo()
    params[2].symbology = frag_sym   
    
    #----------------------------------------------------
    # ATTEMPT TO DELETE INTERMEDIATE FILES AND TEMPORARY WORKSPACE...

    try:
        arcpy.AddMessage("Cleaning up temporary files.")
        arcpy.Delete_management(TempWS)
        #arcpy.Delete_management("f1_ras")
        #arcpy.Delete_management("nf1_ras")
        #arcpy.Delete_management("fpericore")
        #arcpy.Delete_management("frag_edge")
        #arcpy.Delete_management("fragnPatch")
        #arcpy.Delete_management("fpatch0_1")
        #arcpy.Delete_management("fragMap2")
    
    except:
        print "UNABLE TO DELETE TEMPORARY DATA FOLDER: '%s'" % TempWS
        arcpy.AddWarning("UNABLE TO DELETE TEMPORARY DATA FOLDER: '%s'" % TempWS)

 
    e = time.clock()
    print "\nTOTAL TIME IS %s MINUTES" % ((e-s)/60)

except LicenseError:
    arcpy.AddError ("Spatial Analyst license is unavailable")   

except:
    tb = sys.exc_info()[2]
    tbinfo = traceback.format_tb(tb)[0]
    pymsg = tbinfo + "\n" + str(sys.exc_type)+ ": " + str(sys.exc_value)
    arcpy.AddError(pymsg)

    print pymsg

 
