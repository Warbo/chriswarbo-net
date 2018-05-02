with {
  go = stable: (import ./. { inherit stable; }).wholeSite;
};
{
    stable = go true;
  unstable = go false;
}
