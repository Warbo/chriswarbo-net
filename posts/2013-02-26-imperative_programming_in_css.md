---
title: Imperative Programming In CSS
---
In the past week I've been doing the worst form of Web development ever conceived. That's right, HTML emails. I'm doing this on SalesForce, which provides a horrendous mishmash of custom languages for templating and the like. It's not been a pleasant experience, as my Identi.ca stati will attest to.

One problem which produced an interesting solution was adding a column to a table, conditional that at least one of its cells is non-empty. Here's the gist of the problem, using SalesForce's markup and templating languages (called "VisualForce") but with made-up function names:

```xml
<apex:variable var="showThird"
               value="{!anyHasField(Source, "Field_3")}" />
<table>
  <tr>
    <th>Field 1</th>
    <th>Field 2</th>
    <apex:outputPanel rendered="{!showThird}">
      <th>Field 3</th>
    </apex:outputPanel>
    <th>Field 4</th>
  </tr>
  <apex:repeat var="thisElement" value="{!Source.elements}">
    <tr>
      <td>{!thisElement.Field_1}</td>
      <td>{!thisElement.Field_2}</td>
      <apex:outputPanel rendered="{!showThird}">
        <td>{!thisElement.Field_3}</td>
      </apex:outputPanel>
      <td>{!thisElement.Field_4}</td>
    </tr>
  </apex:repeat>
</table>
```

Some things to note:
 - `apex` is the namespace for most of VisualForce's elements.
 - `variable` assigns the contents of its `value` attribute to the name in its `var` attribute. Unfortunately this is a wannabe-imperative construct with bizarre scoping behaviour, rather than a nice, pure "let" construct :(
 - `outputPanel` allows us to wrap a block of markup. It will only be included in the output if its `rendered` attribute is true.
 - `repeat` will output its contents once for each element of the collection given in the `value` attribute. Each element is accessible in its copy via the name given in the `var` attribute.
 - `Source` is just an object which contains the data we need. `Source.elements` means what you would expect in Java (look up the `elements` field of the `Source` object).
 - VisualForce uses a non-XML templating language as well as XML. Expressions in this language start with `{!` and end with `}`. I have absolutely no idea why they don't just define more XML elements.
 - It's acceptable (actually, it's *required*) to nest quotes when using the templating language inside an attribute, as you can see in the `value` of the `showThird` variable.

The problem with the above code is that the set of functions we can use is *incredibly* limited. The `anyHasField` function doesn't exist, I just made it up to get across the problem. How can we get the behaviour we want, given these massive restrictions?

This kind of problem is encountered all of the time when programming. The usual solution is to assume that all of the fields are empty, and change our mind if we see a non-empty field. Imperatively this looks like the following:

```
showThird := FALSE
FOR EACH Source.elements AS thisElement:
  IF nonempty(thisElement.Field_3):
    showThird := TRUE
  ENDIF
ENDFOR
```

We can squash this down a little by noticing that the IF branch has the same behaviour as Boolean OR:

```
showThird := FALSE
FOR EACH Source.elements AS thisElement:
  showThird := showThird OR nonempty(thisElement.Field_3)
ENDFOR
```

However, VisualForce is not an imperative language. Once we define a variable, we cannot redefine it without producing undefined behaviour ("nasal demons").

Of course, I'm a proponent of immutable variables, so this isn't a problem. We can get the same behaviour by folding over Source.elements:

```haskell
showThird = foldl folder False Source.elements
folder result thisElement = result || nonempty (thisElement.Field_3)
```

Unfortunately we can't express our own functions (like `folder` above) in VisualForce, and it certainly doesn't provide the primitives we would need (composition and fold). So, what do?

The solution I've come up with is to use the original imperative solution, but in a language which allows us to re-assign variables. Which language? Cascading Style Sheets (CSS). The elegance of using CSS for the imperative logic is that we have direct control over the display of the elements; our boolean variable is the display property of the elements, with `block` as true and `hidden` as false. This way, we don't need an additional conversion step like `IF showThird THEN block ELSE hidden`. This isn't exactly the same as the original example, since the markup is always included, but it has the same effect and that's all we care about.

We generate our markup as follows:

```xml
<table>
  <tr>
    <th>Field 1</th>
    <th>Field 2</th>
    <th class="third">Field 3</th>
    <th>Field 4</th>
  </tr>
  <apex:repeat var="thisElement" value="{!Source.elements}">
    <tr>
      <td>{!thisElement.Field_1}</td>
      <td>{!thisElement.Field_2}</td>
      <td class="third">{!thisElement.Field_3}</td>
      <td>{!thisElement.Field_4}</td>
    </tr>
  </apex:repeat>
</table>
```

Now we can generate our CSS:

```css
.third {
  display: hidden;
}

<apex:repeat var="thisElement" value="{!Source.elements}">
  <apex:outputPanel rendered="{!nonempty(thisElement.Field_3)}">
    th.third {
      display: block;
    }
    td.third {
      display: block;
    }
  </apex:outputPanel>
</apex:repeat>
```

The first block is always included. It sets elements with the `third` class to hidden. This is equivalent to our `showThird := FALSE` line.

We then loop over all elements, and for any with a non-empty `Field_3` we include a block which sets table headings and cells with the `third` class to visible.

CSS selectors follow a precedence rule, where more-specific selectors will override less-specific ones. We use this precedence to emulate an imperative language; we treat the selector specificity like a line number in BASIC! `td.third` and `th.third` are more specific than `.third`, so they 'run afterwards'. By including these conditionally, we're using an if-condition in our meta-language to emulate an if-condition in our object language (CSS). These blocks may be included many times, but that's perfectly acceptable and results in the same behaviour as a single inclusion.
