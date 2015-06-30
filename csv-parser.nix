{ mkDerivation, base, csv, data-default, MissingH, mtl, safe
, stdenv, transformers, unordered-containers
}:
mkDerivation {
  pname = "csv-parser";
  version = "0.2.2.2";
  src = ./.;
  buildDepends = [
    base csv data-default MissingH mtl safe transformers
    unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
