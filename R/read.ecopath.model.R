#' @title  Input data import function (from an xml file)
#' @usage read.ecopath.model(filename)
#' @description  This function loads input data from an xml file created by the user, or exported from the EwE EcoTroph plug-in, or from a web service associated to a database populated with parameters of several EwE models.
#' @param filename is the address of the file the user wants to import.
#' @seealso check.table to control the reliability of the dataset.
#' @return This function returns a data.frame containing all the column needed to run the EcoTroph R package.
#' @importFrom XML xmlTreeParse
#' @importFrom XML xmlRoot
#' @importFrom XML xmlName
#' @importFrom XML xmlSApply
#' @importFrom XML xmlValue
#' @export

read.ecopath.model <-
function(filename)
{
if(missing(filename))
{
cat("filname is missing\n")
}
else
{


top <- xmlRoot(xmlTreeParse(filename,useInternalNodes=TRUE))
xmlName(top)
names(top)

groupname<-as.vector(xmlSApply(top[["groupname"]],xmlValue))

v<-xmlSApply(top,function(x) as.vector(xmlSApply(x,xmlValue)))

catches_tmp<-xmlSApply(top[["catches"]],function(x) as.numeric(xmlSApply(x,xmlValue)))
catches_tmp2<-data.frame(catches_tmp[1:v$numfleet])[1:length(groupname),]
names(catches_tmp2)<-paste("catch",v$fleetname[-length(v$fleetname)])


#ecopath<-data.frame(v$groupname[-1],as.numeric(v$TL[-1]),as.numeric(v$B[-1]),as.numeric(v$PROD[-1]),as.numeric(v$accessibility[-1]),as.numeric(v$OI[-1]))
ecopath<-data.frame(v$groupname,as.numeric(v$TL),as.numeric(v$B),as.numeric(v$PROD),as.numeric(v$accessibility),as.numeric(v$OI))

names(ecopath)<-c("group_name","TL","biomass","prod","accessibility","OI")

if (is.null(dim(catches_tmp2)))
{
ecopath<-data.frame(ecopath,as.data.frame(catches_tmp2[1:length(rownames(ecopath))]))
names(ecopath)<-c("group_name","TL","biomass","prod","accessibility","OI",paste("catch.",v$fleetname[-length(v$fleetname)],sep='')
)
}
if (!is.null(dim(catches_tmp2)))
{

ecopath<-data.frame(ecopath,as.data.frame(catches_tmp2[1:length(rownames(ecopath)),]))
}
return (ecopath[!(ecopath$group_name==''),])
}
}

