#Host on ShinyApps.io
library(rsconnect)

rsconnect::setAccountInfo(name='andrew-vancil', 
                          token='C5295018FACDBE10CB3895869327DB76', 
                          secret='xywH6Nu2nVbIVRFHqixpdcPmJTe3v98uI7SRFK1l')
rsconnect::deployApp(account = 'andrew-vancil')
