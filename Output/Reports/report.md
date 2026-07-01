Start temperature does not affect copepod CTmax
================
2026-06-30

- [Methods](#methods)
- [Results](#results)
  - [Water temperatures and ramping
    rates](#water-temperatures-and-ramping-rates)
- [CTmax Data](#ctmax-data)
- [References](#references)

Several studies have shown that the temperature a CTmax assay starts at
can affect the observed upper thermal limit (Terblanche et al. 2007).
This seems like it may be species-specific, however, with highly
variable results across species (Faber et al., n.d.), and even across
populations of the same species (Cicchino et al. 2024).

While the effect of start temperature can be quite clear, the underlying
mechanism is still unknown. Much of the past work has been done with
terrestrial organisms (like insects) - in these systems, dehydration is
a major risk during experiments. As a result, the effect of starting
temperature on CTmax may reflect the changes in duration, rather than
the direct effect of a different initial temperature.

In theory, without confounding effects of other processes like
dehydration and starvation, starting temperature should have minimal
effect on CTmax so long as the assay starts below the critical
temperature, or the point where damage accumulation outpaces damage
repair (Faber et al., n.d.). This project examines how CTmax varies with
starting temperature in Skistodiaptomus pallidus, a widespread
freshwater calanoid copepod.

## Methods

We ran CTmax assays starting at different temperatures for several
species, spanning a range of prior conditions.

| Species                   | Acclimation History                                     | Acclimation Temperature     |
|---------------------------|---------------------------------------------------------|-----------------------------|
| Skistodiaptomus pallidus  | 10 generations in laboratory culture                    | 16°C                        |
| Leptodiaptomus siciloides | 1 week under laboratory conditions                      | 23\*C                       |
| Onychodiaptomus birgei    | Field collected (\<5 hours under laboratory conditions) | Variable field temperatures |

We followed a standard protocol for measuring CTmax in copepods.
Briefly, a small water bath was filled with 2 L of water and adjusted to
the desired temperature either by adding ice or with a small aquarium
heater. Once at the correct temperature, copepods were added
individually into 50 mL glass vials with 10 mL of artificial freshwater
medium. Vials were held in the water bath and the water was already
equilibrated to the bath temperature. As such, the transition from
holding temperature to starting temperature was acute.

## Results

### Water temperatures and ramping rates

We tracked temperature in the water bath during one replicate for each
starting temperature to ensure that ramping rates behaved as expected.
Temperature loggers were placed in each water bath and recorded
temperature every 30 seconds (every 1 minute in some cases).

``` r
ggplot(comb_data, aes(x = time_point, y = temp_c, colour = start_temp)) + 
  geom_point() + 
  labs(y = "Water Temp. (°C)",
       colour = "Start Temp. (°C)", 
       x = "Assay Time (minutes)") + 
    theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

Ramping rates (calculated for 5 minute intervals throughout the
experiment) decreased over time, as expected.

``` r

mean_temps = comb_data %>% 
  mutate(minutes = time_point) %>% 
  drop_na(temp_c) %>% 
  group_by(start_temp, ten_min_int) %>% 
  summarise(mean_temp = mean(temp_c))

ramp_rates = comb_data %>% 
  mutate(minutes = time_point) %>% 
  drop_na(temp_c) %>% 
  group_by(start_temp, ten_min_int) %>% 
  nest() %>%
  mutate(
    # Fit the linear model to each interval's data
    model = map(data, ~ lm(temp_c ~ minutes, data = .x)),
    # Tidy the model object into a dataframe
    tidied = map(model, tidy)
  ) %>%
  unnest(tidied) %>%
  # Filter to get just the slope (the coefficient for 'time')
  filter(term == "minutes") %>% 
  inner_join(mean_temps, by = c("start_temp", "ten_min_int"))


ggplot(ramp_rates, aes(x = ten_min_int, y = estimate, colour = start_temp)) + 
  geom_line(linewidth = 2) + 
  labs(y = "Ramp Rate (°C per minute)",
       x = "Ten Minute Interval", 
       colour = "Start Temp. (°C)") + 
    theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

As shown in previous work with this setup, this decrease in ramping
rates was strongly related to the average water temperature during the
time interval, regardless of starting temperature.

``` r

ggplot(ramp_rates, aes(x = mean_temp, y = estimate, colour = start_temp)) + 
  geom_line(linewidth = 2) + 
  labs(y = "Ramp Rate (°C per minute)",
       x = "Average Temp. (°C)", 
       colour = "Start Temp. (°C)") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

## CTmax Data

There was no relationship between observed CTmax and the starting
temperature.

``` r

ggplot(trait_data, aes(x = start_temp, y = ctmax)) + 
  facet_grid(species~.) + 
  geom_point(size = 3) + 
  geom_smooth(method = "lm") + 
  labs(y = "CTmax (°C)",
       x = "Start Temp. (°C)") + 
  theme_matt_facets()
```

<img src="../Figures/markdown/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

This is supported by the results of a linear regression, which shows no
relationship between CTmax and starting temperature.

``` r

model_data = trait_data %>% 
  mutate(ctmax_cent = scale(ctmax, center = T, scale = F),
         start_cent = scale(start_temp, center = T, scale = F))

temp.model = lme4::lmer(data = model_data, 
                ctmax_cent ~ species * start_cent + (1|tube))

performance::check_model(temp.model)

summary(temp.model)
## Linear mixed model fit by REML ['lmerMod']
## Formula: ctmax_cent ~ species * start_cent + (1 | tube)
##    Data: model_data
## 
## REML criterion at convergence: 92.8
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -3.03791 -0.56219  0.00249  0.75499  2.00452 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  tube     (Intercept) 0.0000   0.0000  
##  Residual             0.2867   0.5355  
## Number of obs: 50, groups:  tube, 6
## 
## Fixed effects:
##                                             Estimate Std. Error t value
## (Intercept)                                 0.869000   0.119732   7.258
## speciesSkistodiaptomus pallidus            -1.448333   0.154573  -9.370
## start_cent                                  0.002549   0.019359   0.132
## speciesSkistodiaptomus pallidus:start_cent -0.014549   0.027182  -0.535
## 
## Correlation of Fixed Effects:
##             (Intr) spcsSp strt_c
## spcsSkstdpp -0.775              
## start_cent   0.000  0.000       
## spcsSplld:_  0.000  0.000 -0.712
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see help('isSingular')

car::Anova(temp.model, type = "III")
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: ctmax_cent
##                      Chisq Df Pr(>Chisq)    
## (Intercept)        52.6771  1  3.931e-13 ***
## species            87.7952  1  < 2.2e-16 ***
## start_cent          0.0173  1     0.8952    
## species:start_cent  0.2865  1     0.5925    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
ggplot(trait_data, aes(x = species, y = ctmax)) + 
  geom_boxplot() + 
  geom_point() + 
  labs(x = "Species", 
       y = "CTmax (°C)") + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r

trait_data %>%  
  filter(!is.na(length)) %>% 
  ggplot(aes(x = length, y = ctmax, colour = species)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-cicchino2024" class="csl-entry">

Cicchino, Amanda S., Cameron K. Ghalambor, Brenna R. Forester, Jason D.
Dunham, and W. Chris Funk. 2024. “Greater Plasticity in CTmax with
Increased Climate Variability Among Populations of Tailed Frogs.”
*Proceedings of the Royal Society B: Biological Sciences* 291 (2034):
20241628. <https://doi.org/10.1098/rspb.2024.1628>.

</div>

<div id="ref-faber" class="csl-entry">

Faber, A., F. Møller, B. Ehlers, M. Ørsted, and J. Overgaard. n.d.
“Separating Good from Bad a Methodological Assessment of the Critical
Temperature That Separates Stressful and Permissive Temperatures in
Ectotherms.”

</div>

<div id="ref-terblanche2007" class="csl-entry">

Terblanche, John S, Jacques A Deere, Susana Clusella-Trullas, Charlene
Janion, and Steven L Chown. 2007. “Critical Thermal Limits Depend on
Methodological Context.” *Proceedings of the Royal Society B: Biological
Sciences* 274 (1628): 2935–43. <https://doi.org/10.1098/rspb.2007.0985>.

</div>

</div>
