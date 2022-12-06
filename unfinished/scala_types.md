---
title: Optional Datatype Fields
---

I've been working on a bunch of Scala codebases recently, which represent data
using classes with optional fields. For example (not actual code!):

```scala
case class User(
  userID: String,
  name: Option[String],
  email: Option[String],
  phone: Option[String],
  dateOfBirth: Option[Date],
)
```

There are a few problems with this sort of representation:

 - It's 'stringly typed': using uninformative types like 'String', rather than
   something more specific like 'UserID', 'Email', etc.
 - Using this sort of representation is awkward, e.g.
   `val myUser = User(myID, None, None, None, Some(myDOB))`
 - This representation does not scale well, as we add new fields.

## Rich Types ##

The first problem is easy eough to fix. Values with some particular structure
can use a representation which follows that structure. For example, not every
`String` is a valid email address: there must be two parts separated by '@', and
each part has further restrictions (e.g. the first part cannot be empty, cannot
contain the '@' character, etc.; whilst the second part must be a valid domain
name, like 'localhost', 'gmail.com', etc.). These constraints give rise to
'correct by construction' representations like the following:

```scala
final case class Email(
  private user: Username,
  private host: DomainName,
) {
  override lazy val toString: String = user.toString + '@' + host.toString
}
```

Types which don't have any known structure can use a 'newtype' (borrowing
terminology from Haskell). In Scala we do that with `AnyVal`, like this:

```scala
final case class Name(override val toString: String) extends AnyVal
```

## Smarter Constructors ##
