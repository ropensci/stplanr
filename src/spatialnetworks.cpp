#include <RcppArmadillo.h>
#include <vector>
#include <list>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
List coord_matches(SEXP sldf, double tolval = 0.000) {

  Rcpp::S4 xlines(sldf);
  List lines = xlines.slot("lines");

  unsigned int sllength = lines.length();

  arma::mat x(lines.length()*2,3);

  unsigned int counti = 0;
  for (unsigned int i = 0; i < sllength; i++) {
    List Lines = Rcpp::S4(lines(i)).slot("Lines");
    arma::mat thiscoords = as<arma::mat>(Rcpp::S4(Lines(0)).slot("coords"));
    x(counti, 0) = thiscoords(0,0);
    x(counti, 1) = thiscoords(0,1);
    x(counti, 2) = counti + 1;
    x(counti+1, 0) = thiscoords(thiscoords.n_rows-1,0);
    x(counti+1, 1) = thiscoords(thiscoords.n_rows-1,1);
    x(counti+1, 2) = counti + 2;
    counti = counti + 2;
  }

  double curlat = x(0,0);
  double curlng = x(0,1);
  double curid = x(0,2);

  Environment base("package:base");
  Function order = base["order"];
  arma::uvec sortedidx = as<arma::uvec>(order(wrap(arma::vec(x.col(0))),wrap(arma::vec(x.col(1)))));
  sortedidx = sortedidx - 1;
  arma::mat &sortedxo = x;
  arma::mat sortedx = arma::mat(sortedxo.rows(sortedidx));

  arma::mat matchedcoords(x.n_rows,2);
   curlat = sortedx(0,0);
   curlng = sortedx(0,1);
   curid = sortedx(0,2);
  unsigned int curmatches = 0;
  for (unsigned int i = 1; i < sortedx.n_rows; i++) {
    if (std::abs(sortedx(i,0) - curlat) <= tolval && std::abs(sortedx(i,1) - curlng) <= tolval) {
      matchedcoords(curmatches,0) = curid;
      matchedcoords(curmatches,1) = sortedx(i,2);
      curmatches += 1;
    }
    else {
      curlat = sortedx(i,0);
      curlng = sortedx(i,1);
      curid = sortedx(i,2);
    }
  }
  arma::mat matchedcoords2 = matchedcoords.rows(0,curmatches-1);
  arma::uvec pts = arma::linspace<arma::uvec>(0,x.n_rows-1,x.n_rows);

  if (matchedcoords2.n_rows > 0) {
    for(unsigned int i = 0; i < matchedcoords2.n_rows; i++) {
      pts(matchedcoords2(i,1)-1) = pts(matchedcoords2(i, 0)-1);
    }
  }

  arma::uvec upts = unique(pts);
  arma::uvec pts0(pts.n_rows);
  for (unsigned int i = 0; i < pts.n_rows; i++) {
    pts0(i) = arma::uvec(find(abs(upts - pts(i)) <= tolval,1))(0)+1;
  }
  pts = pts+1;

  arma::uvec node(sllength*2);
  unsigned int countval = 1;
  for (unsigned int i = 0; i < (sllength*2); i+=2) {
    node(i) = countval;
    node(i+1) = countval;
    countval += 1;
  }

  std::vector<arma::uvec> mainlist(upts.n_rows);
  for (unsigned int i = 0; i < upts.n_rows; i++) {
    mainlist[i] = arma::uvec(node.rows(find(pts0 == i+1)));
  }

  return List::create(Named("s")=x,
                      Named("zd")=matchedcoords2,
                      Named("pts")=pts,
                      Named("pts0")=pts0,
                      Named("upts")=upts,
                      Named("nb")=wrap(mainlist));
}

// [[Rcpp::export]]
arma::mat join_spatiallines_coords(SEXP sldf, double startx, double starty) {

  Rcpp::S4 obj(sldf);
  List lines = obj.slot("lines");
  arma::mat fullcoords;

  double prevx = startx;
  double prevy = starty;

  for (unsigned int i = 0; i < lines.length(); i++) {
    List Lines = Rcpp::S4(lines(i)).slot("Lines");
    arma::mat thiscoords = as<arma::mat>(Rcpp::S4(Lines(0)).slot("coords"));
    if (thiscoords(0,0) == prevx && thiscoords(0,1) == prevy) {
      thiscoords = thiscoords.rows((i>0),thiscoords.n_rows-1);
    }
    else {
      thiscoords = thiscoords.rows(arma::linspace<arma::uvec>(thiscoords.n_rows-((i>0)+1), 0, thiscoords.n_rows));
    }
    fullcoords.insert_rows(fullcoords.n_rows, thiscoords);
    prevx = fullcoords(fullcoords.n_rows-1,0);
    prevy = fullcoords(fullcoords.n_rows-1,1);
  }

  return fullcoords;

}



// [[Rcpp::export]]
List coord_matches_sf(arma::mat x, arma::mat sortedx, unsigned int sllength, double tolval = 0.000) {

  // Rcpp::S4 xlines(sldf);
  // List lines = xlines.slot("lines");
  //
  // unsigned int sllength = lines.length();
  //
  // arma::mat x(lines.length()*2,3);
  //
  // unsigned int counti = 0;
  // for (unsigned int i = 0; i < sllength; i++) {
  //   List Lines = Rcpp::S4(lines(i)).slot("Lines");
  //   arma::mat thiscoords = as<arma::mat>(Rcpp::S4(Lines(0)).slot("coords"));
  //   x(counti, 0) = thiscoords(0,0);
  //   x(counti, 1) = thiscoords(0,1);
  //   x(counti, 2) = counti + 1;
  //   x(counti+1, 0) = thiscoords(thiscoords.n_rows-1,0);
  //   x(counti+1, 1) = thiscoords(thiscoords.n_rows-1,1);
  //   x(counti+1, 2) = counti + 2;
  //   counti = counti + 2;
  // }
  //
  double curlat = x(0,0);
  double curlng = x(0,1);
  double curid = x(0,2);
  //
  // Environment base("package:base");
  // Function order = base["order"];
  // arma::uvec sortedidx = as<arma::uvec>(order(wrap(arma::vec(x.col(0))),wrap(arma::vec(x.col(1)))));
  // sortedidx = sortedidx - 1;
  // arma::mat &sortedxo = x;
  // arma::mat sortedx = arma::mat(sortedxo.rows(sortedidx));

  arma::mat matchedcoords(x.n_rows,2);
  curlat = sortedx(0,0);
  curlng = sortedx(0,1);
  curid = sortedx(0,2);
  unsigned int curmatches = 0;
  for (unsigned int i = 1; i < sortedx.n_rows; i++) {
    if (std::abs(sortedx(i,0) - curlat) <= tolval && std::abs(sortedx(i,1) - curlng) <= tolval) {
      matchedcoords(curmatches,0) = curid;
      matchedcoords(curmatches,1) = sortedx(i,2);
      curmatches += 1;
    }
    else {
      curlat = sortedx(i,0);
      curlng = sortedx(i,1);
      curid = sortedx(i,2);
    }
  }
  arma::mat matchedcoords2 = matchedcoords.rows(0,curmatches-1);
  arma::uvec pts = arma::linspace<arma::uvec>(0,x.n_rows-1,x.n_rows);

  if (matchedcoords2.n_rows > 0) {
    for(unsigned int i = 0; i < matchedcoords2.n_rows; i++) {
      pts(matchedcoords2(i,1)-1) = pts(matchedcoords2(i, 0)-1);
    }
  }

  arma::uvec upts = unique(pts);
  arma::uvec pts0(pts.n_rows);
  for (unsigned int i = 0; i < pts.n_rows; i++) {
    pts0(i) = arma::uvec(find(abs(upts - pts(i)) <= tolval,1))(0)+1;
  }
  pts = pts+1;

  arma::uvec node(sllength*2);
  unsigned int countval = 1;
  for (unsigned int i = 0; i < (sllength*2); i+=2) {
    node(i) = countval;
    node(i+1) = countval;
    countval += 1;
  }

  std::vector<arma::uvec> mainlist(upts.n_rows);
  for (unsigned int i = 0; i < upts.n_rows; i++) {
    mainlist[i] = arma::uvec(node(find(pts0 == i+1)));
  }

  return List::create(Named("s")=x,
                      Named("zd")=matchedcoords2,
                      Named("pts")=pts,
                      Named("pts0")=pts0,
                      Named("upts")=upts,
                      Named("nb")=wrap(mainlist));
}


// [[Rcpp::export]]
arma::mat join_spatiallines_coords_sf(List lines, double startx, double starty) {

  arma::mat fullcoords;

  double prevx = startx;
  double prevy = starty;

  for (unsigned int i = 0; i < lines.length(); i++) {
    arma::mat thiscoords = as<arma::mat>(lines(i));
    if (thiscoords(0,0) == prevx && thiscoords(0,1) == prevy) {
      thiscoords = thiscoords.rows((i>0),thiscoords.n_rows-1);
    }
    else {
      thiscoords = thiscoords.rows(arma::linspace<arma::uvec>(thiscoords.n_rows-((i>0)+1), 0, thiscoords.n_rows));
    }
    fullcoords.insert_rows(fullcoords.n_rows, thiscoords);
    prevx = fullcoords(fullcoords.n_rows-1,0);
    prevy = fullcoords(fullcoords.n_rows-1,1);
  }

  return fullcoords;

}
