library(tidyverse)
library(shiny)
library(readxl)
library(scales)
library(grid)
library(lubridate)
library(stringr)

# Main Data (raw)
fefo_df <- readxl::read_excel("FEFO Report.xlsx")

fefo_df %>% 
  dplyr::mutate(branch = as.numeric(branch)) -> fefo_df


                  
# Calculate last Monday and last Sunday dates
last_monday <- format(Sys.Date() - as.numeric(format(Sys.Date(), "%u")) - 6, "%Y%m%d")
last_sunday <- format(Sys.Date() - as.numeric(format(Sys.Date(), "%u")), "%Y%m%d")

# Convert report_dt to character format to ensure correct comparison
fefo_df %>% 
  dplyr::mutate(report_dt = as.character(report_dt)) %>%
  dplyr::filter(report_dt >= last_monday & report_dt <= last_sunday) %>% 
  dplyr::group_by(branch, report_dt, ship_dt, cus_alpha_cd, ship_to_alpha_cd, doc_no, item_no, item_desc, uom, per_pallet, lot_no, 
                  exp_dt, ssl, ssl_exp_dt, shipped_by, FEFO_issue_lot, FEFO_issue_lot_exp_dt, FEFO_issue_lot_ssl_exp_dt, 
                  FEFO_issue_onHand, new_reason_cd, new_reason_desc) %>% 
  dplyr::summarise(std_cost = sum(std_cost),
                   qty_shipped = sum(qty_shipped),
                   FEFO_issue_lot_Cost = sum(FEFO_issue_lot_Cost)) %>% 
  dplyr::rename(Branch = branch,
                "Report Date" = report_dt,
                "Pick Date" = ship_dt,
                "Sold To Name" = cus_alpha_cd,
                "Ship To Name" = ship_to_alpha_cd,
                "Ord_no" = doc_no,
                SKU = item_no,
                "SKU Description" = item_desc,
                UoM = uom,
                "Cs Per Pallet" = per_pallet,
                "LOT# PICKED" = lot_no,
                "Expired On" = exp_dt,
                "Shipable Shelf Life (days)" = ssl,
                "SSL Expired On" = ssl_exp_dt,
                "Pick User" = shipped_by,
                "Lot To Be Picked" = FEFO_issue_lot,
                "Lot To Be Picked Exp Date" = FEFO_issue_lot_exp_dt,
                "Lot To Be Picked SSL Exp Dt" = FEFO_issue_lot_ssl_exp_dt,
                "Lot To Be Picked onHand" = FEFO_issue_onHand,
                "Reason Cd" = new_reason_cd,
                "Reason Description" = new_reason_desc,
                "Std Cost" = std_cost,
                "Qty Picked" = qty_shipped,
                "Lot To Be Picked Cost Risk" = FEFO_issue_lot_Cost)-> eod_fefo_report



