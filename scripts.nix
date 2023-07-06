{s}: rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:gossip-glomers' --allow-eval --warnings";
  hoogleScript = s "hgl" "hoogle serve";
}
