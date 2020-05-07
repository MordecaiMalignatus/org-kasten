{ nixpkgs ? import <nixpkgs> {}}:

with nixpkgs.emacsPackages;

melpaBuild rec {
  pname = "org-kasten";
  packageRequires = [ s dash ];
  version = "0.1";
  src = ./org-kasten.el;
  recipe = builtins.toFile "recipe" ''
  (${pname} :fetcher github :repo "mordecaimalignatus/${pname}" :files ("org-kasten.el"))
  '';
}
