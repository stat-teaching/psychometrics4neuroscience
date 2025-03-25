library(tidyverse)
library(lme4)

# https://osf.io/zhtbj/?view_only=

dat <- read.csv("emoint.csv")

# conversion for responses
# 1 = neutral
# 2 = anger
# 3 = disgust
# 4 = fear
# 5 = happiness
# 6 = sadness
# 7 = surprise

resp_code <- c(
  "1" = "neutral",
  "2" = "anger",
  "3" = "disgust",
  "4" = "fear",
  "5" = "happiness",
  "6" = "sadness",
  "7" = "suprise"
)

emotion_code <- c(
  "fear" = "fear",
  "disop" = "disgust",
  "discl" = "disgust",
  "hap" = "happiness",
  "sad" = "sadness",
  "sur" = "suprise",
  "angcl" = "anger",
  "neutral" = "neutral"
)

dat_clean <- dat |> 
  pivot_longer(4:ncol(dat), values_to = "response") |> 
  separate(name, into = c("face", "emotion", "intensity"), sep = "_")  |>
  # intensity as number
  mutate(intensity = as.numeric(intensity)) |> 
  # neutral as maximal intensity, avoid NA
  mutate(intensity = ifelse(emotion == "neutral", 100, intensity))

names(dat_clean)[1:3] <- c("id", "gender", "age")

# recoding response with labels
# see https://adv-r.hadley.nz/subsetting.html?q=look#lookup-tables

dat_clean$response_lbl <- resp_code[dat_clean$response]

# recoding the displayed emotion as the response

dat_clean$emotion_lbl <- emotion_code[dat_clean$emotion]

# binary accuracy if emotion_lbl == response_lbl

dat_clean$acc <- as.integer(dat_clean$response_lbl == dat_clean$emotion_lbl)

# centering intensity on the minimum, thus 0 is the 10% intensity

dat_clean$intensity0 <- dat_clean$intensity - min(dat_clean$intensity)

dat_clean_sad <- filter(dat_clean, emotion_lbl == "sadness")

fit <- glmer(acc ~ intensity0 + (intensity0|id), 
             data = dat_clean_sad, 
             family = binomial(link = "logit"))

fe <- as.matrix(vcov(fit))
re <- ranef(fit,condVar=TRUE)
reM <- apply(attr(re[[1]],"postVar"), 3, function(x) x, simplify = FALSE)
reF <- lapply(reM, function(m) fe + m)

cc <- coefficients(fit)[[1]]

res <- vector(mode = "list", length = nrow(cc))

for(i in 1:nrow(cc)){
    bb <- c(cc[i, 1], cc[i, 2])
    names(bb) <- names(cc)
    pse <- car::deltaMethod(bb, "-(Intercept)/intensity0", reF[[i]])
    jnd <- car::deltaMethod(bb, "1/intensity0", reF[[i]])
    
    pse <- data.frame(pse)
    jnd <- data.frame(jnd)
    pse$param <- "pse"
    jnd$param <- "jnd"
    
    res[[i]] <- rbind(pse, jnd)
}

res <- do.call(rbind, res)
rownames(res) <- NULL
names(res) <- c("estimate", "se", "ci.lb", "ci.ub", "param")
res$id <- 1:nrow(res)

# res |> 
#     ggplot(aes(x = estimate, y = id)) +
#     geom_point() +
#     geom_segment(aes(x = ci.lb, xend = ci.ub, y = id, yend = id)) +
#     facet_wrap(~param, scales = "free")
# 

datl <- split(dat_clean_sad, dat_clean_sad$id)
fitl <- lapply(datl, function(d) glm(acc ~ intensity0, data = d, family = binomial(link = "logit")))

resl <- vector(mode = "list", length = length(fitl))

for(i in 1:length(fitl)){
    pse <- car::deltaMethod(fitl[[i]], "-(Intercept)/intensity0")
    jnd <- car::deltaMethod(fitl[[i]], "1/intensity0")
    pse <- data.frame(pse)
    jnd <- data.frame(jnd)
    pse$param <- "pse"
    jnd$param <- "jnd"
    resl[[i]] <- rbind(pse, jnd)
}

resl <- do.call(rbind, resl)
rownames(resl) <- NULL
names(resl) <- c("estimate", "se", "ci.lb", "ci.ub", "param")
resl$id <- 1:nrow(resl)

res$method <- "lme4"
resl$method <- "glm"

resd <- rbind(res, resl)

PSE <- car::deltaMethod(fit, "-(Intercept)/intensity0")
JND <- car::deltaMethod(fit, "1/intensity0")

resd$avg <- ifelse(resd$param == "pse", PSE[1, 1], JND[1, 1])
resd$avg_lb <- ifelse(resd$param == "pse", PSE[1, 3], JND[1, 3])
resd$avg_ub <- ifelse(resd$param == "pse", PSE[1, 4], JND[1, 4])

resd |>
    ggplot(aes(x = estimate, y = id, color = method)) +
    geom_pointrange(aes(xmin = ci.lb, xmax = ci.ub),
                    position = position_dodge(width = 2)) +
    facet_wrap(~param, scales = "free") +
    geom_vline(aes(xintercept = avg)) +
    geom_vline(aes(xintercept = avg_lb), lty = "dashed") +
    geom_vline(aes(xintercept = avg_ub), lty = "dashed")
