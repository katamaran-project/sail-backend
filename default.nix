{ lib
, buildDunePackage
, nanosail
, sail
}:

buildDunePackage {
  pname = "sail_katamaran_backend";
  version = "0.1";

  src = ./.;

  buildInputs = [
    nanosail
  ];

  propagatedBuildInputs = [
    sail
  ];

  meta = with lib; {
    homepage = "https://github.com/katamaran-project/sail-backend";
    description = "Sail backend for the Katamaran project";
    maintainers = with maintainers; [ skeuchel ];
    license = licenses.bsd2;
  };
}
