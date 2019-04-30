#' @get /ntm/<proposal_id>/<account_id>

function(proposal_id, account_id) {
  
  options(scipen = 200,
          mongodb = list(
            "host" = "mongodb://127.0.0.1:27017"
          ))
  
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(mongolite)
  library(jsonlite)
  
  print(getwd())
  
  source("./Functions.R", encoding = "UTF-8")
  
  # proposal_id <- "5c7ce8b1421aa9907926eb71"
  # account_id <- "5c4552455ee2dd7c36a94a9e"
  
  if (!missing(account_id) & !missing(proposal_id)) {
    
    db_paper <- mongo(collection = "Paper", db = "pharbers-ntm-client", url = options()$mongodb$host)
    paper_info <- db_paper$find(query = paste0('{"proposal-id" : "', proposal_id, '", "account-id" : "', account_id, '"}'))
    input_ids <- paper_info$`input-ids`[[1]]
    
    db_paper_input <- mongo(collection = "Paperinput", db = "pharbers-ntm-client", url = options()$mongodb$host)
    phase_info <- db_paper_input$find(query = paste0('{"_id" : {"$oid" : "', input_ids[length(input_ids)], '"}}'), fields = '{"_id" : 1, "phase" : 1}')
    phase <- phase_info$phase
    
    curves <- get_intermedia(uuid = "intermedia", type = "curves")
    weightages <- get_intermedia(uuid = "intermedia", type = "weightages")
    
    p_data <- get_p_data(paper_info = paper_info)
    input_data <- get_input_data(paper_info = paper_info)
    
    dat <- get_data2use(p_data = p_data, input_data = input_data)
    
    ## results
    results <- get_results(dat = dat, weightages = weightages)
    
    ## reports
    hosp_report <- get_hosp_report(results = results)
    rep_report <- get_rep_report(results = results)
    prod_report <- get_prod_report(results = results)
    
    ## output reports
    proposal_id <- paper_info$`proposal-id`
    paper_input_ids <- paper_info$`input-ids`[[1]]
    paper_input_id <- paper_input_ids[length(paper_input_ids)]
    
    db_scenario <- mongo(collection = "Scenario", db = "pharbers-ntm-client", url = options()$mongodb$host)
    scenario_info <- db_scenario$find(query = paste0('{"proposal-id" : "', proposal_id, '", "phase" : ', format(phase, nsmall = 1), '}'), fields = '{}')
    scenario_id <- scenario_info$`_id`
    
    db_hosp_report <- mongo(collection = "HospitalSalesReport", db = "pharbers-ntm-client", url = options()$mongodb$host)
    
    db_hosp_report$insert(hosp_report, 
                          na = "string", 
                          auto_unbox = TRUE)
    
    hospital_sales_report_ids <- tail(db_hosp_report$find(query = '{}', fields = '{"_id" : 1}'), nrow(hosp_report))$`_id`
    
    db_rep_report <- mongo(collection = "RepresentativeSalesReport", db = "pharbers-ntm-client", url = options()$mongodb$host)
    
    db_rep_report$insert(rep_report, 
                         na = "string", 
                         auto_unbox = TRUE)
    
    representative_sales_report_ids <- tail(db_rep_report$find(query = '{}', fields = '{"_id" : 1}'), nrow(rep_report))$`_id`
    
    db_prod_report <- mongo(collection = "ProductSalesReport", db = "pharbers-ntm-client", url = options()$mongodb$host)
    
    db_prod_report$insert(prod_report, 
                          na = "string", 
                          auto_unbox = TRUE)
    
    product_sales_report_ids <- tail(db_prod_report$find(query = '{}', fields = '{"_id" : 1}'), nrow(prod_report))$`_id`
    
    db_sales_report <- mongo(collection = "SalesReport", db = "pharbers-ntm-client", url = options()$mongodb$host)
    
    db_sales_report$update(query = paste0('{"time" : "0"}'), 
                           update = paste0('{"$set" : {"scenario-id" : ', toJSON(scenario_id, auto_unbox = TRUE), 
                                           ', "hospital-sales-report-ids" : ', toJSON(as.list(hospital_sales_report_ids), auto_unbox = TRUE), 
                                           ', "representative-sales-report-ids" : ', toJSON(as.list(representative_sales_report_ids), auto_unbox = TRUE), 
                                           ', "product-sales-report-ids" : ', toJSON(as.list(product_sales_report_ids), auto_unbox = TRUE), 
                                           ', "paper-input-id" : ', toJSON(paper_input_id, auto_unbox = TRUE), 
                                           ', "time" : ', as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")), '}}'), 
                           upsert = TRUE)
    
    ## update information
    personnel_assessment_ids <- paper_info$`personnel-assessment-ids`[[1]]
    db_personnel_assessment <- mongo(collection = "PersonnelAssessment", db = "pharbers-ntm-client", url = options()$mongodb$host)
    personnel_assessment_info <- db_personnel_assessment$find(query = paste0('{"_id" : {"$oid" : "', personnel_assessment_ids[length(personnel_assessment_ids)], '"}}'))
    p_rep_ability_ids <- personnel_assessment_info$`representative-ability-ids`[[1]]
    p_action_kpi_ids <- personnel_assessment_info$`action-kpi-ids`[[1]]
    
    db_rep_ability <- mongo(collection = "RepresentativeAbility", db = "pharbers-ntm-client", url = options()$mongodb$host)
    db_action_kpi <- mongo(collection = "ActionKpi", db = "pharbers-ntm-client", url = options()$mongodb$host)
    
    p_action_kpi <- data.frame()
    for (i in 1:length(p_action_kpi_ids)) {
      info <- db_action_kpi$find(query = paste0('{"_id" : {"$oid" : "', p_action_kpi_ids[i], '"}}'))
      p_action_kpi <- bind_rows(p_action_kpi, info)
    }
    
    rep_ability <- get_rep_ability(results = results)
    action_kpi <- get_action_kpi(p_action_kpi = p_action_kpi, rep_ability = rep_ability)
    
    db_rep_ability$insert(rep_ability, 
                          na = "string", 
                          auto_unbox = TRUE)
    
    rep_ability_ids <- tail(db_rep_ability$find(query = '{}', fields = '{"_id" : 1}'), nrow(rep_ability))$`_id`
    
    db_action_kpi$insert(action_kpi, 
                         na = "string", 
                         auto_unbox = TRUE)
    
    action_kpi_ids <- tail(db_action_kpi$find(query = '{}', fields = '{"_id" : 1}'), nrow(action_kpi))$`_id`
    
    db_personnel_assessment$update(query = paste0('{"time" : "0"}'), 
                                   update = paste0('{"$set" : {"representative-ability-ids" : ', toJSON(rep_ability_ids, auto_unbox = TRUE), 
                                                   ', "action-kpi-ids" : ', toJSON(action_kpi_ids, auto_unbox = TRUE), 
                                                   ', "paper-input-id" : ', toJSON(paper_input_id, auto_unbox = TRUE), 
                                                   ', "scenario-id" : ', toJSON(scenario_id, auto_unbox = TRUE), 
                                                   ', "time" : ', as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")), '}}'), 
                                   upsert = TRUE)
    
    ## update paper
    sales_report_ids <- paper_info$`sales-report-ids`[[1]]
    sales_report_id_new <- tail(db_sales_report$find(query = paste0('{"scenario-id" : "', scenario_id, '", "paper-input-id" : "', paper_input_id, '"}'), fields = '{"_id" : 1}'), 1)$`_id`
    personnel_assessment_id_new <- tail(db_personnel_assessment$find(query = paste0('{"scenario-id" : "', scenario_id, '", "paper-input-id" : "', paper_input_id, '"}'), fields = '{"_id" : 1}'), 1)$`_id`
    
    db_paper$update(query = paste0('{"proposal-id" : "', proposal_id, '", "account-id" : "', account_id, '"}'), 
                    update = paste0('{"$set" : {"sales-report-ids" : ', toJSON(c(sales_report_ids, sales_report_id_new), auto_unbox = TRUE), 
                                    ', "personnel-assessment-ids" : ', toJSON(c(personnel_assessment_ids, personnel_assessment_id_new), auto_unbox = TRUE), '}}'), 
                    upsert = FALSE)
    
    ## output
    return("Done")
    
  } else {
    
    return("Failed: proposal_id or account_id missed. ")
  }
}






























