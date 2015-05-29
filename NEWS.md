# Version 0.2.0 (Milestone)
### Major changes
- Reworked the Volkov distribution for performance.
- Reworked density, quantile and distribution functions for more consistent behaviour, 
error handling and large performance gains.
- Added some missing functions for mzsm, poig and poix families.
- New function *updatesad* to update a fitsad with better fit from profile.
- *radpred* now uses exact solutions for extreme data points.
- Reimplemented the *AIC* and *AICc* functions for better integration with fitsad and fitrad classes.
- Removed the trueLL methods to an experimental branch

### Enhancements
- Improved grammar and clearer text in some manual pages and vignette.
- Improved starting values for several distributions.
- Replaced table(cut(x)) for hist for improved performance.
- octav and rad classes now have validation functions and are more consistent.
- More informative calls in *show* and *summary*.
- Reduced code redundancy and improved readability.

### Bugfixes
- Fixed the Broken-Stick family to behave like a Discrete distribution.
- Small but importante fixes in *octavpred*.
- Workarounds for bugs and issues of the bbmle package.
- Fixed a bug that caused *plotprofmle* to crash

# Version 0.1.10
- Initial release on CRAN.
