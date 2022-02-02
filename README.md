# Miscellaneous Code

A repository for potentially useful R and Python functions and analysis examples that are not
intended for publication in reviewed libraries.

# Available code:

| Code                         | Language | Description                                                                             |
|:-----------------------------|:---------|:----------------------------------------------------------------------------------------|
| `BackAzimuth`                   | R | Calculates a back azimuth
| `BinaryMatrix`                  | R | Creates NxN binary matrix with upper and lower triangle probabilities  
| `biserial.cor`                  | R | Measures the strength and direction of the association between a continuous and binary variable 
| `blockStatistics`               | R | Calculates raster block statistics, similar to focal but the result is the focal block and not focal pixel 
| `BlusResiduals`                 | R | Removes autocorrelation in regression residuals for best, unbiased, linear, scalar (BLUS) 
| `BooleanDistMatrix`             | R | Example for finding neighbors within specified distance using boolean matrix where distance condition is <= d TRUE else FALSE 
| `bootCorTest`                   | R | Test bootstrap independence of a variable 
| `BootstrapImputation`           | R | Bayesian Bootstrap Imputation  
| `box_muller`                    | R | Box-Muller transformation 
| `cAIC`                          | R | Conditional AIC where one is interested in the particular coefficient estimates for individual random effects
| `ChangePointDetection`          | R | Examples of univariate and multivariate change point detection 
| `ClassificationImprovedSqError` | R | Evaluate Classification Improved Squared Error
| `color.transparency`            | R | Creates new HEX value with transparency
| `cRamp`                         | R | Creates a new vector based on a color ramp
| `CreateDistributions`           | R | Example of creating a few distributions (eg., normal, skewed, kurtotic) 
| `DiffUnequalVectors`            | R | Comparison of unequal grouping factor lengths
| `DiffUnequalVectors`            | R | Example of calculating statistics when class lengths are not equal
| `fragmentation`                 | R | Ritter et al fragmentation index
| `GaussianMixtureModel`          | R | Example of a Gaussian mixture model 
| `GeographicallyWeightedPCA`     | R | Conduct a Geographically Weighted PCA
| `HilbertSpaceFillingCurve`      | R | Function for generating a Hilbert Space Filling Curve
| `hli.pt`                        | R | Point estimate for Heat Load Index
| `joincount_multi`               | R | multi-class joincount based on nearest neighbor or distance, used to explore the spatial structure of binomial data 
| `krige.back.transform`          | R | Back-transformation of a log-transformed Kriging estimate 
| `lee_filter`                    | R | Lee smoothing filter for InSAR data
| `libraries`                     | R | Function to manage R libraries
| `logit.transform`               | R | Logit transformation for a variable with a known lower and upper limit (eg., p=0-1, 0-100%)
| `MakeSampleGrid`                | R | Example of making regular point sample grid
| `mandelbrot`                    | R | Mandelbrot's Fractal Dimension 
| `matrixEquality`                | R | Evaluation of the equality of two covariance matrices
| `mean.unit.angle`               | R | Mean angle within a unit (polygon) for slope or aspect in degrees 
| `misc_lmetrics`                 | R | Functions for contagion and IJI metrics
| `morans.poly`                   | R | Moran's-I for 1st order polygon contingency  
| `multiring_buffer`              | R | Function for multi-ring buffer
| `multivariate.norm.surf`        | R | Examples of creating multivariate normal surfaces 
| `odds.ratio`                    | R | Odds ratio for 2x2 contingency 
| `parse.formula`                 | R | Function to create a formula object
| `PctImageDiff`                  | R | Percent difference between two nominal rasters
| `polarHistogram`                | R | Function for creating polar histogram 
| `PolygonContingency`            | R | Examples of building Nth order polygon contingency Wij matrices  
| `qr.gramschmidt`                | R | The Gram-Schmidt QR matrix decomposition 
| `rad2area`                      | R | Some area-radius conversions
| `ranger_proximity`              | R | Function to create ranger random forest proximity matrix
| `rbind.unequal`                 | R | rbind data.frames with unequal columns
| `removeHoles`                   | R | Remove holes from an sp SpatialPolygons object
| `sinuosity`                     | R | Build node data 
| `slope_lesson`                  | R | Code examples on how to calculate/code Zevenbergen slope from a DEM
| `spatial_estimate_uncertanity`  | R | Example of estimating spatial uncertainty form ranger random forests model
| `squareBuffer`                  | R | Create a square buffer
| `terrain.attributes`            | R | Derives the Zevenbergen, Evans, Shary or, Moore terrain attributes; slope, aspect, profile and planform curvatures
| `tri.col.legend`                | R | Creates a three-color triangle color gradient legend
| `viewshed`                      | R | Viewshed analysis
| `xtabs.chi`                     | R | cross-tabulation contengency matrix with Chi-square test
| `weightedAIC`                   | R | AIC weights for model averaging and multimodel inference
| `weightedCentroids`             | R | Parameter weighted centroids
| `viewshed`                      | R | Viewshed analysis
| `GUIDOS`                        | Python | GUIDOS landscape fragmentation index (requires ArcPy and ESRI spatial analysit license)
		
**Help**: Users are encouraged to as questions regarding code here [issues](https://github.com/jeffreyevans/GeNetIt/issues) in the menu above, and press new issue to start a new bug report, documentation correction or feature request. You can direct questions to <jeffrey_evans@tnc.org>.

















  


