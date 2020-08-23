updateCodes = function(df){
  #' This function modifies old codes and names for the most up to date 
  #' correspnding ones so that aggregation can be performed the right way
  
  #edge cases
  ## Folkestone and Hythe & Shepway, same code different name Shepway -> F&H
  if (any(df$areaName == 'Shepway')){
    df[df$areaName == 'Shepway', 'areaName'] = 'Folkestone and Hythe'
    df[df$areaName == 'Folkestone and Hythe', 'areaCode'] = 'E07000112' # F&H code
  }
  
  # Dorset and Dorset, Dorset got its code changed in 2019
  ## new code: E06000059; old code: 	E10000009
  if(any(df$areaCode == 'E10000009')){
    df[df$areaCode == 'E10000009', 'areaCode'] = 'E06000059'
  } 
  
  ## East Suffolk(new) = Suffolk Coastal(old) + Waveney (old)
  if(any(df$areaName == 'Suffolk Coastal' | df$areaName == 'Waveney')){
    df[df$areaName == 'Suffolk Coastal', 'areaName'] = 'East Suffolk'
    df[df$areaName == 'Waveney', 'areaName'] = 'East Suffolk'
    
    df[df$areaName == 'East Suffolk', 'areaCode'] = 'E07000244'
  }
  
  ##West suffolk (new) = Forest Heath(old) + St Edmunsdbury district(old)
  if(any(df$areaName == 'Forest Heath' | df$areaName == 'St Edmundsbury')){
    df[df$areaName == 'Forest Heath', 'areaName'] = 'West Suffolk'
    df[df$areaName == 'St Edmundsbury', 'areaName'] = 'West Suffolk'
    
    df[df$areaName == 'West Suffolk', 'areaCode'] = 'E07000245'
  }
  
  ## Somerset West and Taunton (new) ; Taunton Deane(old) and West Somerset(old)
  # confusing: check https://en.wikipedia.org/wiki/Somerset_West_and_Taunton
  if(any(df$areaName == 'Taunton Deane' | df$areaName == 'West Somerset')){
    df[df$areaName == 'Taunton Deane', 'areaName'] = 'Somerset West and Taunton'
    df[df$areaName == 'West Somerset', 'areaName'] = 'Somerset West and Taunton'
    
    df[df$areaName == 'Somerset West and Taunton', 'areaCode'] = 'E07000246'
  }
  ## the only thing missing is taking into account these changes and combining the values
  # where the area code and the area name are the same, I could either just add them together 
  # or average them depending on what sort of metric we are talking about
  # If I wanted to be more precise I should weight it by the population
  return(df)
}