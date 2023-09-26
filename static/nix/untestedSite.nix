# Combine all pages together into a directory
{ attrsToDirs', stripOverrides, merge, blog, projects, resources, unfinished }:
attrsToDirs' "untestedSite"
(stripOverrides (merge [ blog projects resources unfinished ]))
