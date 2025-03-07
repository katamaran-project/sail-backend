{ lib
, buildDunePackage
, base
, odoc
}:

buildDunePackage {
  pname = "monads";
  version = "0.1";

  src = ./.;

  buildInputs = [
    base
    odoc
  ];

  meta = with lib; {
    homepage = "https://github.com/katamaran-project/sail-backend";
    description = "Sail backend for the Katamaran project - Monad Library";
    maintainers = with maintainers; [ skeuchel ];
    license = licenses.bsd2;
  };
}
