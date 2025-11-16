{ lib
, buildNpmPackage
, fetchFromGitHub
, nodejs
, makeWrapper
}:

buildNpmPackage rec {
  pname = "claude-code-acp";
  version = "0.10.0";

  src = fetchFromGitHub {
    owner = "zed-industries";
    repo = "claude-code-acp";
    rev = "v${version}";
    hash = "sha256-ZbCumFZyGFoNBNK6PC56oauuN2Wco3rlR80/1rBPORk="; # Replace with actual hash after first build
  };

  npmDepsHash = "sha256-nzP2cMYKoe4S9goIbJ+ocg8bZPY/uCTOm0bLbn4m6Mw="; # Replace with actual hash after first build

  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    # Ensure the binary is properly wrapped with Node.js
    makeWrapper ${nodejs}/bin/node $out/bin/claude-code-acp \
      --add-flags $out/lib/node_modules/@zed-industries/claude-code-acp/dist/index.js
  '';

  meta = with lib; {
    description = "Use Claude Code from any ACP client such as Zed";
    homepage = "https://github.com/zed-industries/claude-code-acp";
    license = licenses.mit;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
