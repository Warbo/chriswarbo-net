# Provide all pages, with all of our tests as dependencies
{ allDrvsIn, siteTests, untestedSite, withDeps' }:

withDeps' "chriswarbo.net" (allDrvsIn siteTests) untestedSite
