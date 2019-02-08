#' Build eTUFF metadata header
#'
#' Takes typical row of metadata spreadsheet and creates standardized eTUFF
#' header for use in Tagbase.
#'
#' @param meta_row is a row of meta spreadsheet for the deployment of interest
#' @param filename is output filename for this eTUFF file. Needs to be .txt.
#' @param global_att is character vector of additional global attributes to be
#'   associated with this eTUFF file. See the example for required formatting of
#'   vector elements.
#'
#' @return nothing to console. header is written to disk.
#' @export
#' @examples
#' \dontrun{
#' # build_meta_head uses input tag metadata to build a header similar to:
#' cat("header1\nheader2\n", file="filename.csv")
#' cat("header3\n", file="filename.csv", append=TRUE)
#' DF <- data.frame(a=1:2, b=c("a","B"))
#' write.table(DF, file="filename.csv", append=TRUE)
#' > header1
#' header2
#' header3
#' "a" "b"
#' "1" 1 "a"
#' "2" 2 "B"
#'
#' g_atts <- c('institution = "WHOI"', 'references = "doi: xxxx"')
#' build_meta_head(meta_row = meta[1,], filename = 'eTUFF_example.txt', global_attributes = g_atts)
#' }

build_meta_head <- function(meta_row, filename, global_attributes=NULL){

  # melt cols to rows
  meta.new <- reshape2::melt(meta, id.vars=c('uid_no'))

  # filter out NA and other missing values
  meta.new <- meta.new[which(!is.na(meta.new$value)),]
  meta.new <- meta.new[which(meta.new$value != ''),]

  # merge with possible meta types
  meta.new <- merge(x = meta.new, y = metaTypes[ , c("Category","AttributeID", 'AttributeName')],
                    by.x = 'variable', by.y = "AttributeName", all.x=TRUE)

  # are we missing any essential vars?
  if (any(!(metaTypes$AttributeID[which(metaTypes$Necessity == 'required')] %in% meta.new$AttributeID))){
    idx <- which(metaTypes$Necessity == 'required')
    idx <- idx[!(metaTypes$AttributeID[idx] %in% meta.new2$AttributeID)]
    missingVars <- metaTypes[idx,]
    stop('missing required variables. The missing vars have been output as "missingVars".')
  }

  meta.new <- meta.new[,c('Category','AttributeID','variable','value')]
  names(meta.new) <- c('Category','AttributeID','AttributeName','AttributeValue')
  meta.new <- meta.new[which(!is.na(meta.new$Category)),]

  # format to etuff header

  ## add global atts, if any
  hdr <- paste('// global attributes:',sep='')
  for (ii in 1:length(global_attributes)){
    hdr[(length(hdr)+1)] <- paste('  :', global_attributes[ii], sep='')
  }

  idx <- which(meta.new$Category == 'instrument')
  if (length(idx) > 0){
    hdr[(length(hdr)+1)] <- paste('// etag device attributes:', sep='')
    for (ii in idx){
      hdr[(length(hdr)+1)] <- paste('  :', meta.new$AttributeName[ii], ' = ', '\"', meta.new$AttributeValue[ii], '\"', sep='')
    }
  }

  idx <- which(meta.new$Category == 'attachment')
  if (length(idx) > 0){
    hdr[(length(hdr)+1)] <- paste('// etag attachment attributes:', sep='')
    for (ii in idx){
      hdr[(length(hdr)+1)] <- paste('  :', meta.new$AttributeName[ii], ' = ', '\"', meta.new$AttributeValue[ii], '\"', sep='')
    }
  }

  idx <- which(meta.new$Category == 'deployment')
  if (length(idx) > 0){
    hdr[(length(hdr)+1)] <- paste('// etag deployment attributes:', sep='')
    for (ii in idx){
      hdr[(length(hdr)+1)] <- paste('  :', meta.new$AttributeName[ii], ' = ', '\"', meta.new$AttributeValue[ii], '\"', sep='')
    }
  }

  idx <- which(meta.new$Category == 'end_of_mission')
  if (length(idx) > 0){
    hdr[(length(hdr)+1)] <- paste('// etag end of mission attributes:', sep='')
    for (ii in idx){
      hdr[(length(hdr)+1)] <- paste('  :', meta.new$AttributeName[ii], ' = ', '\"', meta.new$AttributeValue[ii], '\"', sep='')
    }
  }

  idx <- which(meta.new$Category == 'animal')
  if (length(idx) > 0){
    hdr[(length(hdr)+1)] <- paste('// etag animal attributes:', sep='')
    for (ii in idx){
      hdr[(length(hdr)+1)] <- paste('  :', meta.new$AttributeName[ii], ' = ', '\"', meta.new$AttributeValue[ii], '\"', sep='')
    }
  }

  idx <- which(meta.new$Category == 'waypoints')
  if (length(idx) > 0){
    hdr[(length(hdr)+1)] <- paste('// etag waypoints attributes:', sep='')
    for (ii in idx){
      hdr[(length(hdr)+1)] <- paste('  :', meta.new$AttributeName[ii], ' = ', '\"', meta.new$AttributeValue[ii], '\"', sep='')
    }
  }

  idx <- which(meta.new$Category == 'quality')
  if (length(idx) > 0){
    hdr[(length(hdr)+1)] <- paste('// etag quality attributes:', sep='')
    for (ii in idx){
      hdr[(length(hdr)+1)] <- paste('  :', meta.new$AttributeName[ii], ' = ', '\"', meta.new$AttributeValue[ii], '\"', sep='')
    }
  }

  ## skip file attributes?

  # add data header
  hdr[(length(hdr)+1)] <- paste('// data:', sep='')
  hdr[(length(hdr)+1)] <- paste('// DateTime,VariableID,VariableValue,VariableName,VariableUnits', sep='')


  cat(hdr, file = filename, sep='\n')

  # later append the data to this header like below:
  #write.table(argos.new, file='try-meta-hdr.txt', sep=',', append=TRUE, col.names = F, row.names = F, quote = F)


}
