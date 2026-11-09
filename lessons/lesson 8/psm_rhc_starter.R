# =============================================================================
#  Propensity Score Matching with EHR Data — STARTER SCRIPT
#  성향점수 매칭 실습 — 시작 스크립트
#
#  Put this file and sim_severity.csv in your RStudio project folder.
#  이 파일과 sim_severity.csv를 RStudio 프로젝트 폴더에 넣으세요.
#
#  Everything after the "YOUR CODE STARTS HERE" line, you write yourself,
#  following the slides.  아래 "YOUR CODE STARTS HERE" 이후는 직접 작성합니다.
# =============================================================================


# ---- Step 0a · Install packages (run ONCE) · 패키지 설치 (한 번만) -----------
# Remove the "#" at the start of the next lines and run them once:
# install.packages(c(
#   "tidyverse", "MatchIt", "optmatch", "cobalt", "gtsummary", "gt", "survey", "smd",
#   "marginaleffects", "sandwich", "broom", "patchwork", "EValue", "scales"
# ))


# ---- Step 0b · Load all packages · 패키지 불러오기 ---------------------------
library(tidyverse)        # dplyr, ggplot2, readr, tidyr, forcats ...
library(MatchIt)          # matchit(), match.data()
library(optmatch)         # optimal / full matching (used by MatchIt)
library(cobalt)           # bal.tab(), love.plot(), bal.plot()
library(gtsummary)        # tbl_summary(), tbl_svysummary(), tbl_merge()
library(gt)               # table rendering
library(survey)           # svydesign() for weighted tables
library(smd)              # standardized mean differences (used by add_smd())
library(marginaleffects)  # avg_comparisons()
library(sandwich)         # robust variance estimators
library(broom)            # tidy()
library(patchwork)        # combine plots: p1 + p2
library(EValue)           # evalues.RR()
library(scales)           # percent()
set.seed(2026)            # reproducible results


# ---- Step 0c · Load and prepare the RHC data · 자료 준비 ---------------------
url <- "https://hbiostat.org/data/repo/rhc.csv"
if (!file.exists("rhc.csv")) download.file(url, destfile = "rhc.csv")   # download once
rhc <- read.csv("rhc.csv")

rhc <- rhc |>
  mutate(
    treat  = as.integer(swang1 == "RHC"),   # treatment 처치: 1 = RHC, 0 = no RHC
    died30 = as.integer(dth30  == "Yes"),   # outcome  결과: 1 = died within 30 days
    swang1 = factor(swang1, levels = c("No RHC", "RHC")),
    across(c(cat1, ca, sex, race, income, ninsclas, dnr1, resp, card, neuro,
             gastr, renal, meta, hema, seps, trauma, ortho), factor)
  )

# The 50 baseline covariates used by Connors et al. (provided so you don't have to type them)
# 연구에 사용된 50개 기저 공변량 (직접 입력하지 않아도 되도록 제공)
covs <- c("age","sex","race","edu","income","ninsclas","cat1","das2d3pc","dnr1",
          "ca","surv2md1","aps1","scoma1","wtkilo1","temp1","meanbp1","resp1",
          "hrt1","pafi1","paco21","ph1","wblc1","hema1","sod1","pot1","crea1",
          "bili1","alb1","resp","card","neuro","gastr","renal","meta","hema",
          "seps","trauma","ortho","cardiohx","chfhx","dementhx","psychhx",
          "chrpulhx","renalhx","liverhx","gibledhx","malighx","immunhx",
          "transhx","amihx")


# ---- Step 0d · Helper for Table 1 · 기초특성표 도우미 함수 --------------------
# Adds an SMD column to a gtsummary table (tbl_summary or tbl_svysummary).
# Use it at the end of a pipe:  tbl_summary(...) |> add_p() |> add_smd()
add_smd <- function(tbl) {
  smd_fun <- function(data, variable, by, ...) {
    w <- NULL
    if (inherits(data, "survey.design")) { w <- weights(data); data <- data$variables }
    tibble::tibble(smd = smd::smd(x = data[[variable]], g = data[[by]], w = w)$estimate)
  }
  tbl |>
    add_stat(fns = everything() ~ smd_fun) |>
    modify_header(smd ~ "**SMD**") |>
    modify_fmt_fun(smd ~ label_style_number(digits = 2))
}


# =============================================================================
#  YOUR CODE STARTS HERE · 여기부터 직접 작성하세요
# =============================================================================

# ---- Step 0 · Explore the data (dim, glimpse, count, mean) ------------------


# ---- Part 1 · Simulation (sim_severity.csv) ---------------------------------


# ---- Part 1 · Exact matching with 1, 3, and 10 variables --------------------


# ---- Step 1 · Covariates, Table 1, SMD, naive comparison --------------------


# ---- Step 2 · Propensity score model and overlap ----------------------------


# ---- Step 3 · Matching -------------------------------------------------------


# ---- Step 4 · Balance diagnostics --------------------------------------------


# ---- Step 5 · Treatment effect (RD, RR, doubly robust) ----------------------


# ---- Step 6 · IPTW -----------------------------------------------------------


# ---- Comparing methods & E-value ---------------------------------------------

