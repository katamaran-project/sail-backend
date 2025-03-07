{ lib
, buildDunePackage
, monads
, pprint
, ppx_here
, sail
}:

buildDunePackage {
  pname = "nanosail";
  version = "0.1";

  src = ./.;

  propagatedBuildInputs = [
    monads
    pprint
    ppx_here
    sail
  ];

  meta = with lib; {
    homepage = "https://github.com/katamaran-project/sail-backend";
    description = "Sail backend for the Katamaran project - NanoSail IR";
    maintainers = with maintainers; [ skeuchel ];
    license = licenses.bsd2;
  };
}
