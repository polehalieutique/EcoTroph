\name{convert.list2tab}
\alias{convert.list2tab}
\title{
convert.list2tab
}
\description{
convert.list2tab enables to convert the list object returned by the create.ETdiagnosis function into a list of data.frames. These data.frames contain calculated variables by TL class and combinations of effort multipliers.
}
\usage{
convert.list2tab(diagn.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{diagn.list}{
is the list object returned by the create.ETdiagnosis function. 
}
}
\value{
This function returns a list of data.frames : B, B_acc, P, P_acc, Kin, Kin_acc, Y, Fish_mort, Fish_mort_acc and ET_Main_diagnose.
}
\author{
Gatti Paul, Colleter Mathieu, Guitton Jerome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
create.ETdiagnosis 
}
\examples{
data(ecopath_guinee)
\donttest{Liste=create.ETdiagnosis(create.ETmain(ecopath_guinee))
Tab=convert.list2tab(Liste)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~EcoTroph }
\keyword{ ~Diagnosis }