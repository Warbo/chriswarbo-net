---
title: Projective Geometric Algebra
---

Projective geometry is a particularly simple form of geometry: it consists only
of points and lines; the only equipment needed (other than pen & paper) is a
straight-edge; the only skills needed are to identify where two lines cross, and
to draw a line between two points.

Although projective geometry is limited, this actually makes it more *general*.
For example, it works in exactly the same way for a flat piece of paper, for the
spherical geometry of astronomy and cartography, and for the hyperbolic geometry
of relativity; since it doesn't involve notions like "angle", "distance",
"parallel" etc. which behave very differently across those regimes.

## Constructing a Number Line ##

Draw two points and a line between them:

```{pipe="cat > params.json"}
{
  "width"   : 500,
  "height"  : 500,
  "leftInf" : [0.05, 0.05],
  "rightInf": [0.95, 0.05],
  "origin"  : [0.4 , 0.9 ]
}
```

<svg width="300" height="200">
  <script type="text/javascript">
  function makeDraggable(evt) {
    var svg = evt.target;

    svg.addEventListener('mousedown', startDrag);
    svg.addEventListener('mousemove', drag);
    svg.addEventListener('mouseup', endDrag);
    svg.addEventListener('mouseleave', endDrag);
    svg.addEventListener('touchstart', startDrag);
    svg.addEventListener('touchmove', drag);
    svg.addEventListener('touchend', endDrag);
    svg.addEventListener('touchleave', endDrag);
    svg.addEventListener('touchcancel', endDrag);

    var selectedElement, offset, transform,
        bbox, minX, maxX, minY, maxY, confined;

    var boundaryX1 = 10.5;
    var boundaryX2 = 30;
    var boundaryY1 = 2.2;
    var boundaryY2 = 19.2;

    function getMousePosition(evt) {
      var CTM = svg.getScreenCTM();
      if (evt.touches) { evt = evt.touches[0]; }
      return {
        x: (evt.clientX - CTM.e) / CTM.a,
        y: (evt.clientY - CTM.f) / CTM.d
      };
    }

    function startDrag(evt) {
      if (evt.target.classList.contains('draggable')) {
        selectedElement = evt.target;
        offset = getMousePosition(evt);

        // Make sure the first transform on the element is a translate transform
        var transforms = selectedElement.transform.baseVal;

        if (transforms.length === 0 || transforms.getItem(0).type !== SVGTransform.SVG_TRANSFORM_TRANSLATE) {
          // Create an transform that translates by (0, 0)
          var translate = svg.createSVGTransform();
          translate.setTranslate(0, 0);
          selectedElement.transform.baseVal.insertItemBefore(translate, 0);
        }

        // Get initial translation
        transform = transforms.getItem(0);
        offset.x -= transform.matrix.e;
        offset.y -= transform.matrix.f;

        confined = evt.target.classList.contains('confine');
        if (confined) {
            bbox = selectedElement.getBBox();
            minX = boundaryX1 - bbox.x;
            maxX = boundaryX2 - bbox.x - bbox.width;
            minY = boundaryY1 - bbox.y;
            maxY = boundaryY2 - bbox.y - bbox.height;
        }
      }
    }

    function drag(evt) {
      if (selectedElement) {
        evt.preventDefault();

        var coord = getMousePosition(evt);
        var dx = coord.x - offset.x;
        var dy = coord.y - offset.y;

        if (confined) {
            if (dx < minX) { dx = minX; }
            else if (dx > maxX) { dx = maxX; }
            if (dy < minY) { dy = minY; }
            else if (dy > maxY) { dy = maxY; }
        }

        transform.setTranslate(dx, dy);
      }
    }

    function endDrag(evt) {
      selectedElement = false;
    }
  }
  </script>
  <line x1="0" y1="10" x2="110" y2="10" stroke="black" />
  <circle cx="10"  cy="10" r="5" />
  <circle cx="100" cy="10" r="5" />
</svg>


Projective geometry can be expressed using cross products
(e.g. see https://youtu.be/puMYfJTFdgQ?t=964 )

Cross products are icky. Can we use geometric products instead?

Going further: projective geometry involves lines and points, which are
dual/symmetric.  Geometric algebra doesn't involve points or (unbounded) lines;
it starts with vectors.  What's the nicest way to capture these things? Is it by
going to a higher dimension, like the homogeneous coordinates of projective
geometry (e.g. subspaces crossing a "viewing plane"?)?

ANSWER: Projective Geometric Algebra can define points as the intersection of
lines. Dually, lines can be defined as the join of points. Likewise for lines
with planes, planes with volumes, etc. in as many dimensions as we like.

Objects defined via joins have a linear orientation; e.g. the join of points A
and B is a line through A and B, directed from A to B; the join of lines C and D
is a plane containing A and B, DIRECTED HOW? DO WE NEED A 'RIGHT-HAND RULE'?
These objects are invariant in the numberof dimensions.

Objects defined via meets have a rotational orientation; e.g.the meet of planes
A and B is a line lying in both (their intersection). Treating this line as an
axis, its orientation is the angle between A and B (maybe better with a dot
product definition?)

Conformal geometric algebra has objects representing spheres. The projective
'line at infinity' can be used as a radius, giving planes ('infinite spheres'),
lines (infinite circles), etc. Points are spheres with zero radius.

The fundamental operation in geometric algebra is reflection. Chaining together
reflections can implement rotations (of any magnitude, about any axis);
rotations about a 'point at infinity' are translations. Reflections in conformal
geometric algebra are inversions.

The inverse of a vector, in geometric algebra, points in the same direction, but
its magnitude is the reciprocal. Projective GA makes this well-defined for zero,
by giving the line at infinity (I think?).
