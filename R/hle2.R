#' Hodges-Lehmann Estimate of Shift
#'
#' @description
#' `outer_hle2` computes the Hodges-Lehmann Shift Estimator of two vectors
#'
#' @details
#' This particular implementation of hle2 is computed using the R Core `outer`
#' function. Outer is less performant when compared to the C++ method.
#' The estimate should be roughly equivalent, though.
#'
#' @param x Numeric Vector containing the first set of data
#'
#' @param y Numeric Vector containing the second set of data
#'
#' @returns Numeric Estimate of the shift of location parameter
#'
#' @examples
#' library(datar)
#' x <- rnorm(100, 10) # Random Normal with Location Parameter of 10
#' y <- rnorm(100, 20) # Random Normal with Location Parameter of 20
#' outer_hle2(x, y)
#'
#' @seealso [cpp_hle2()] The faster, C++, implementation of this function
#'
#' @references
#' DATA 495 Lectures at Northern Michigan University by John Kloke
#' @export
outer_hle2 <- function(x, y) {
    outer_vec <- outer(y, x, "-")
    return(median(outer_vec))
}

#' Tree Coverage Type Dataset
#'
#' A dataset which is aimed at describing the cover type of a forest with
#'  cartographic variables only
#'
#' # Description From Dataset Publishers:
#' Predicting forest cover type from cartographic variables only
#' (no remotely sensed data). The actual forest cover type for a
#' given observation
#'  (30 x 30 meter cell) was determined from US Forest Service (USFS) Region 2
#' Resource Information System (RIS) data. Independent variables were derived
#' from data originally obtained from US Geological Survey (USGS)
#' and USFS data.
#' Data is in raw form (not scaled) and contains binary (0 or 1)
#' columns of data for qualitative independent variables (wilderne
#' ss areas and soil types).
#' This study area includes four wilderness areas located in the
#' Roosevelt National
#' Forest of northern Colorado. These areas represent forests with
#' minimal human-caused disturbances, so that existing forest cover types
#' are more a result of ecological processes rather than forest
#' management practices.

#' Some background information for these four wilderness areas:
#' - Neota (area 2) probably has the highest mean elevational v
#' alue of the 4 wilderness areas.
#' Rawah (area 1) and Comanche Peak (area 3)
#' would have a lower mean elevational value, while Cache la Poudre (area 4)
#' would have the lowest mean elevational value.
#' As for primary major tree species in these areas,
#'  Neota would have spruce/fir (type 1), while Rawah and
#'  Comanche Peak would probably have lodgepole pine (type 2)
#'  as their primary species, followed by spruce/fir and aspen
#'  (type 5). Cache la Poudre would tend to have Ponderosa pine
#' (type 3), Douglas-fir (type 6), and cottonwood/willow (type 4).
#'
#' \itemize{
#'  \item Elevation in Meters
#'  \item Aspect
#'  \item Horizontal Distance to Hydrology
#'  \item Vertical Distance to Hydrology
#'  \item Hillshade 9am
#'  \item Hillshade Noon
#'  \item Hillshade 3pm
#'  \item Horizontal Distance to Fire Points
#'  \item Wilderness Area
#'  \item Soil Type
#'  \item Cover Type
#' }
#' This data as well as more information is in the dataset.
#' @name covertype
#'
#' @docType data
#'
#' @keywords data
#'
#' @usage
#' data(covertype)
#'
#' @references \url{https://www.kaggle.com/datasets/impapan/forest-covertype-dataset}
"covertype"

#' Proton-Proton Collision Dataset
#'
#' @description
#' A dataset which shows records the data from Proton-Proton collsions at CERN
#'
#' @details
#' # From the Dataset Publishers
#' ## Context
#' MultiJet primary dataset in AOD format from RunB of 2010.
#' This dataset contains runs from 2010 RunB. The list of all validated runs,
#' which must be applied to all analyses, can be found in
#' \url{http://opendata.cern.ch/record/1000}
#' This file contains events from the MultiJet primary dataset from
#' the CMS open data release, and computes the razor variables MR and Rsq,
#' used in supersymmetric particle searches. More details on the razor
#' variables can be found in Phys. Rev. D 90, 112001
#' \url{https://journals.aps.org/prd/pdf/10.1103/PhysRevD.90.112001}
#' ## Content
#' \itemize{
#' \item Run: The run number of the event.
#' \item Lumi: The lumi section of the event.
#' \item Event: The event number of the event.
#' \item MR: First razor kinematic variable, the MR variable is an
#'      estimate of an overall mass scale, which in the limit of massless
#'      decay products equals the mass of the heavy parent particle.
#' \item MR: First razor kinematic variable, the MR variable is
#'      an estimate of an overall mass scale, which in the limit of
#'      massless decay products equals the mass of the heavy parent particle.
#' \item Rsq: Second razor kinematic variable, the Rsq variable is
#'       the square of the ratio R, which quantifies the flow of
#'        energy in the plane perpendicular to the beam and the partitioning
#'        of momentum between visible and invisible particles.
#' \item E1,Px1,Py1,Pz1: The four-vector of the leading megajet
#'      (with the largest transverse momentum).
#' \item E2,Px2,Py2,Pz2: The four-vector of the subleading megajet
#'      (with the largest transverse momentum).
#' \item HT: The scalar sum of the transverse momentum of the jets.
#' \item MET: The magnitude of the vector sum of the transverse energy of
#'      the particles in the event.
#' \item nJets: The number of jets with transverse momentum above 40 GeV.
#' \item nBJets: The number of b-tagged jets with transverse momentum
#'      above 40 GeV.
#' }
#' @name proton-proton collisions
#'
#' @docType data
#'
#' @keywords data
#'
#' @usage
#' data(pp_collision)
#'
#' @references \url{https://www.kaggle.com/datasets/fedesoriano/multijet-primary-dataset/data}
"pp_collision"
