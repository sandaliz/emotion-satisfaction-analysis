# Individual Findings: Music Analysis 

## Dataset Overview
- **Dataset**: Emotify - Induced Musical Emotion Dataset
- **Source**: Aljanaki et al., 2014 (Technical Report UU-CS-2014-015)
- **Collection Method**: Online game with a purpose (GWAP)
- **Year Collected**: 2013
- **Original Sample**: 1,595 participants, 400 songs

## Data Cleaning
- **Initial rows**: 8,407 listening events
- **After cleaning**: 8,255 events (removed ages <10 or >90, and 140 duplicates)
- **Final sample**: 8,255 independent listening events

## Statistical Profile
| Metric | Value |
|--------|-------|
| Total Listening Events | 8,255 |
| Mean Age (SD) | 31.87 (12.25) |
| Age Range | 10-82 |
| Gender Distribution | 45.4% Female, 54.6% Male |
| Mean Emotional Intensity (SD) | 1.93 (0.86) |
| Overall Satisfaction Rate | 39.4% |

## Descriptive Analytics
Satisfaction increases monotonically with emotional connection:
- **Lowest emotional quartile**: 33.7% satisfaction
- **Highest emotional quartile**: 46.3% satisfaction
- **Gap**: +12.6 percentage points

By genre:
- Classical: 48.0% satisfaction
- Rock: 37.7% satisfaction
- Pop: 35.1% satisfaction
- Electronic: 33.2% satisfaction

## Inferential Analytics
- **Pearson correlation**: r = 0.159 (95% CI [0.138, 0.180]), p < .001
- **Spearman correlation**: ρ = 0.156, p < .001
- **T-test (high vs low emotion)**: t(4961.7) = 12.28, p < .001
- **Cohen's d**: 0.295 (small-medium effect)
- **ANOVA**: Significant differences across quartiles, F(3,8251) = 32.15, p < .001

## Predictive Analytics
Multiple regression with controls:
- **Model R²**: 3.4% of variance explained
- **Strongest predictor**: Emotional intensity (β = 0.092, p < .001)
- **Genre effect**: Classical music significantly higher satisfaction (β = 0.129)
- **Test set RMSE**: 0.4805

## Interpretation
The music dataset provides **support for the statement**. While the effect size is small to medium (d = 0.295), the relationship is statistically significant across all three analytical approaches. The monotonic increase in satisfaction across emotional quartiles suggests a dose-response relationship: the more emotions felt, the higher the satisfaction.