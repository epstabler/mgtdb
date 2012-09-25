'''Here is a ported version of nltk.tree and nltk.draw which can be packaged
and used with the Python 3 version of the code
--all code has dependency on in_idle, which is included in this file'''

import sys

from tkinter import (Button, Canvas, Entry, Frame, IntVar, Label, Menu, Menubutton,
                     RAISED, Scrollbar, StringVar, Text, Tk, Toplevel, Widget)

import tkinter.font, tkinter.messagebox, tkinter.filedialog

##//////////////////////////////////////////////////////
##  Various widgets extracted from nltk.draw.util
##//////////////////////////////////////////////////////

class CanvasWidget(object):
    """
    A collection of graphical elements and bindings used to display a
    complex object on a Tkinter ``Canvas``.  A canvas widget is
    responsible for managing the ``Canvas`` tags and callback bindings
    necessary to display and interact with the object.  Canvas widgets
    are often organized into hierarchies, where parent canvas widgets
    control aspects of their child widgets.

    Each canvas widget is bound to a single ``Canvas``.  This ``Canvas``
    is specified as the first argument to the ``CanvasWidget``'s
    constructor.

    Attributes
    ==========
    Each canvas widget can support a variety of "attributes", which
    control how the canvas widget is displayed.  Some typical examples
    attributes are ``color``, ``font``, and ``radius``.  Each attribute
    has a default value.  This default value can be overridden in the
    constructor, using keyword arguments of the form
    ``attribute=value``:

        >>> from nltk.draw.util import CanvasText
        >>> cn = CanvasText(c, 'test', color='red')

    Attribute values can also be changed after a canvas widget has
    been constructed, using the ``__setitem__`` operator:

        >>> cn['font'] = 'times'

    The current value of an attribute value can be queried using the
    ``__getitem__`` operator:

        >>> cn['color']
        red

    For a list of the attributes supported by a type of canvas widget,
    see its class documentation.

    Interaction
    ===========
    The attribute ``'draggable'`` controls whether the user can drag a
    canvas widget around the canvas.  By default, canvas widgets
    are not draggable.

    ``CanvasWidget`` provides callback support for two types of user
    interaction: clicking and dragging.  The method ``bind_click``
    registers a callback function that is called whenever the canvas
    widget is clicked.  The method ``bind_drag`` registers a callback
    function that is called after the canvas widget is dragged.  If
    the user clicks or drags a canvas widget with no registered
    callback function, then the interaction event will propagate to
    its parent.  For each canvas widget, only one callback function
    may be registered for an interaction event.  Callback functions
    can be deregistered with the ``unbind_click`` and ``unbind_drag``
    methods.

    Subclassing
    ===========
    ``CanvasWidget`` is an abstract class.  Subclasses are required to
    implement the following methods:

      - ``__init__``: Builds a new canvas widget.  It must perform the
        following three tasks (in order):
          - Create any new graphical elements.
          - Call ``_add_child_widget`` on each child widget.
          - Call the ``CanvasWidget`` constructor.
      - ``_tags``: Returns a list of the canvas tags for all graphical
        elements managed by this canvas widget, not including
        graphical elements managed by its child widgets.
      - ``_manage``: Arranges the child widgets of this canvas widget.
        This is typically only called when the canvas widget is
        created.
      - ``_update``: Update this canvas widget in response to a
        change in a single child.

    For a ``CanvasWidget`` with no child widgets, the default
    definitions for ``_manage`` and ``_update`` may be used.

    If a subclass defines any attributes, then it should implement
    ``__getitem__`` and ``__setitem__``.  If either of these methods is
    called with an unknown attribute, then they should propagate the
    request to ``CanvasWidget``.

    Most subclasses implement a number of additional methods that
    modify the ``CanvasWidget`` in some way.  These methods must call
    ``parent.update(self)`` after making any changes to the canvas
    widget's graphical elements.  The canvas widget must also call
    ``parent.update(self)`` after changing any attribute value that
    affects the shape or position of the canvas widget's graphical
    elements.

    :type __canvas: Tkinter.Canvas
    :ivar __canvas: This ``CanvasWidget``'s canvas.

    :type __parent: CanvasWidget or None
    :ivar __parent: This ``CanvasWidget``'s hierarchical parent widget.
    :type __children: list(CanvasWidget)
    :ivar __children: This ``CanvasWidget``'s hierarchical child widgets.

    :type __updating: bool
    :ivar __updating: Is this canvas widget currently performing an
        update?  If it is, then it will ignore any new update requests
        from child widgets.

    :type __draggable: bool
    :ivar __draggable: Is this canvas widget draggable?
    :type __press: event
    :ivar __press: The ButtonPress event that we're currently handling.
    :type __drag_x: int
    :ivar __drag_x: Where it's been moved to (to find dx)
    :type __drag_y: int
    :ivar __drag_y: Where it's been moved to (to find dy)
    :type __callbacks: dictionary
    :ivar __callbacks: Registered callbacks.  Currently, four keys are
        used: ``1``, ``2``, ``3``, and ``'drag'``.  The values are
        callback functions.  Each callback function takes a single
        argument, which is the ``CanvasWidget`` that triggered the
        callback.
    """
    def __init__(self, canvas, parent=None, **attribs):
        """
        Create a new canvas widget.  This constructor should only be
        called by subclass constructors; and it should be called only
        "after" the subclass has constructed all graphical canvas
        objects and registered all child widgets.

        :param canvas: This canvas widget's canvas.
        :type canvas: Tkinter.Canvas
        :param parent: This canvas widget's hierarchical parent.
        :type parent: CanvasWidget
        :param attribs: The new canvas widget's attributes.
        """
        if self.__class__ == CanvasWidget:
            raise TypeError('CanvasWidget is an abstract base class')

        if not isinstance(canvas, Canvas):
            raise TypeError('Expected a canvas!')

        self.__canvas = canvas
        self.__parent = parent

        # If the subclass constructor called _add_child_widget, then
        # self.__children will already exist.
        if not hasattr(self, '_CanvasWidget__children'): self.__children = []

        # Is this widget hidden?
        self.__hidden = 0

        # Update control (prevents infinite loops)
        self.__updating = 0

        # Button-press and drag callback handling.
        self.__press = None
        self.__drag_x = self.__drag_y = 0
        self.__callbacks = {}
        self.__draggable = 0

        # Set up attributes.
        for (attr, value) in attribs.items(): self[attr] = value

        # Manage this canvas widget
        self._manage()

        # Register any new bindings
        for tag in self._tags():
            self.__canvas.tag_bind(tag, '<ButtonPress-1>',
                                   self.__press_cb)
            self.__canvas.tag_bind(tag, '<ButtonPress-2>',
                                   self.__press_cb)
            self.__canvas.tag_bind(tag, '<ButtonPress-3>',
                                   self.__press_cb)

    ##//////////////////////////////////////////////////////
    ##  Inherited methods.
    ##//////////////////////////////////////////////////////

    def bbox(self):
        """
        :return: A bounding box for this ``CanvasWidget``. The bounding
            box is a tuple of four coordinates, *(xmin, ymin, xmax, ymax)*,
            for a rectangle which encloses all of the canvas
            widget's graphical elements.  Bounding box coordinates are
            specified with respect to the coordinate space of the ``Canvas``.
        :rtype: tuple(int, int, int, int)
        """
        if self.__hidden: return (0,0,0,0)
        if len(self.tags()) == 0: raise ValueError('No tags')
        return self.__canvas.bbox(*self.tags())

    def width(self):
        """
        :return: The width of this canvas widget's bounding box, in
            its ``Canvas``'s coordinate space.
        :rtype: int
        """
        if len(self.tags()) == 0: raise ValueError('No tags')
        bbox = self.__canvas.bbox(*self.tags())
        return bbox[2]-bbox[0]

    def height(self):
        """
        :return: The height of this canvas widget's bounding box, in
            its ``Canvas``'s coordinate space.
        :rtype: int
        """
        if len(self.tags()) == 0: raise ValueError('No tags')
        bbox = self.__canvas.bbox(*self.tags())
        return bbox[3]-bbox[1]

    def parent(self):
        """
        :return: The hierarchical parent of this canvas widget.
            ``self`` is considered a subpart of its parent for
            purposes of user interaction.
        :rtype: CanvasWidget or None
        """
        return self.__parent

    def child_widgets(self):
        """
        :return: A list of the hierarchical children of this canvas
            widget.  These children are considered part of ``self``
            for purposes of user interaction.
        :rtype: list of CanvasWidget
        """
        return self.__children

    def canvas(self):
        """
        :return: The canvas that this canvas widget is bound to.
        :rtype: Tkinter.Canvas
        """
        return self.__canvas

    def move(self, dx, dy):
        """
        Move this canvas widget by a given distance.  In particular,
        shift the canvas widget right by ``dx`` pixels, and down by
        ``dy`` pixels.  Both ``dx`` and ``dy`` may be negative, resulting
        in leftward or upward movement.

        :type dx: int
        :param dx: The number of pixels to move this canvas widget
            rightwards.
        :type dy: int
        :param dy: The number of pixels to move this canvas widget
            downwards.
        :rtype: None
        """
        if dx == dy == 0: return
        for tag in self.tags():
            self.__canvas.move(tag, dx, dy)
        if self.__parent: self.__parent.update(self)

    def moveto(self, x, y, anchor='NW'):
        """
        Move this canvas widget to the given location.  In particular,
        shift the canvas widget such that the corner or side of the
        bounding box specified by ``anchor`` is at location (``x``,
        ``y``).

        :param x,y: The location that the canvas widget should be moved
            to.
        :param anchor: The corner or side of the canvas widget that
            should be moved to the specified location.  ``'N'``
            specifies the top center; ``'NE'`` specifies the top right
            corner; etc.
        """
        x1,y1,x2,y2 = self.bbox()
        if anchor == 'NW': self.move(x-x1,        y-y1)
        if anchor == 'N':  self.move(x-x1/2-x2/2, y-y1)
        if anchor == 'NE': self.move(x-x2,        y-y1)
        if anchor == 'E':  self.move(x-x2,        y-y1/2-y2/2)
        if anchor == 'SE': self.move(x-x2,        y-y2)
        if anchor == 'S':  self.move(x-x1/2-x2/2, y-y2)
        if anchor == 'SW': self.move(x-x1,        y-y2)
        if anchor == 'W':  self.move(x-x1,        y-y1/2-y2/2)

    def destroy(self):
        """
        Remove this ``CanvasWidget`` from its ``Canvas``.  After a
        ``CanvasWidget`` has been destroyed, it should not be accessed.

        Note that you only need to destroy a top-level
        ``CanvasWidget``; its child widgets will be destroyed
        automatically.  If you destroy a non-top-level
        ``CanvasWidget``, then the entire top-level widget will be
        destroyed.

        :raise ValueError: if this ``CanvasWidget`` has a parent.
        :rtype: None
        """
        if self.__parent is not None:
            self.__parent.destroy()
            return

        for tag in self.tags():
            self.__canvas.tag_unbind(tag, '<ButtonPress-1>')
            self.__canvas.tag_unbind(tag, '<ButtonPress-2>')
            self.__canvas.tag_unbind(tag, '<ButtonPress-3>')
        self.__canvas.delete(*self.tags())
        self.__canvas = None

    def update(self, child):
        """
        Update the graphical display of this canvas widget, and all of
        its ancestors, in response to a change in one of this canvas
        widget's children.

        :param child: The child widget that changed.
        :type child: CanvasWidget
        """
        if self.__hidden or child.__hidden: return
        # If we're already updating, then do nothing.  This prevents
        # infinite loops when _update modifies its children.
        if self.__updating: return
        self.__updating = 1

        # Update this CanvasWidget.
        self._update(child)

        # Propagate update request to the parent.
        if self.__parent: self.__parent.update(self)

        # We're done updating.
        self.__updating = 0

    def manage(self):
        """
        Arrange this canvas widget and all of its descendants.

        :rtype: None
        """
        if self.__hidden: return
        for child in self.__children: child.manage()
        self._manage()

    def tags(self):
        """
        :return: a list of the canvas tags for all graphical
            elements managed by this canvas widget, including
            graphical elements managed by its child widgets.
        :rtype: list of int
        """
        if self.__canvas is None:
            raise ValueError('Attempt to access a destroyed canvas widget')
        tags = []
        tags += self._tags()
        for child in self.__children:
            tags += child.tags()
        return tags

    def __setitem__(self, attr, value):
        """
        Set the value of the attribute ``attr`` to ``value``.  See the
        class documentation for a list of attributes supported by this
        canvas widget.

        :rtype: None
        """
        if attr == 'draggable':
            self.__draggable = value
        else:
            raise ValueError('Unknown attribute %r' % attr)

    def __getitem__(self, attr):
        """
        :return: the value of the attribute ``attr``.  See the class
            documentation for a list of attributes supported by this
            canvas widget.
        :rtype: (any)
        """
        if attr == 'draggable':
            return self.__draggable
        else:
            raise ValueError('Unknown attribute %r' % attr)

    def __repr__(self):
        """
        :return: a string representation of this canvas widget.
        :rtype: str
        """
        return '<%s>' % self.__class__.__name__

    def hide(self):
        """
        Temporarily hide this canvas widget.

        :rtype: None
        """
        self.__hidden = 1
        for tag in self.tags():
            self.__canvas.itemconfig(tag, state='hidden')

    def show(self):
        """
        Show a hidden canvas widget.

        :rtype: None
        """
        self.__hidden = 0
        for tag in self.tags():
            self.__canvas.itemconfig(tag, state='normal')

    def hidden(self):
        """
        :return: True if this canvas widget is hidden.
        :rtype: bool
        """
        return self.__hidden

    ##//////////////////////////////////////////////////////
    ##  Callback interface
    ##//////////////////////////////////////////////////////

    def bind_click(self, callback, button=1):
        """
        Register a new callback that will be called whenever this
        ``CanvasWidget`` is clicked on.

        :type callback: function
        :param callback: The callback function that will be called
            whenever this ``CanvasWidget`` is clicked.  This function
            will be called with this ``CanvasWidget`` as its argument.
        :type button: int
        :param button: Which button the user should use to click on
            this ``CanvasWidget``.  Typically, this should be 1 (left
            button), 3 (right button), or 2 (middle button).
        """
        self.__callbacks[button] = callback

    def bind_drag(self, callback):
        """
        Register a new callback that will be called after this
        ``CanvasWidget`` is dragged.  This implicitly makes this
        ``CanvasWidget`` draggable.

        :type callback: function
        :param callback: The callback function that will be called
            whenever this ``CanvasWidget`` is clicked.  This function
            will be called with this ``CanvasWidget`` as its argument.
        """
        self.__draggable = 1
        self.__callbacks['drag'] = callback

    def unbind_click(self, button=1):
        """
        Remove a callback that was registered with ``bind_click``.

        :type button: int
        :param button: Which button the user should use to click on
            this ``CanvasWidget``.  Typically, this should be 1 (left
            button), 3 (right button), or 2 (middle button).
        """
        try: del self.__callbacks[button]
        except: pass

    def unbind_drag(self):
        """
        Remove a callback that was registered with ``bind_drag``.
        """
        try: del self.__callbacks['drag']
        except: pass

    ##//////////////////////////////////////////////////////
    ##  Callback internals
    ##//////////////////////////////////////////////////////

    def __press_cb(self, event):
        """
        Handle a button-press event:
          - record the button press event in ``self.__press``
          - register a button-release callback.
          - if this CanvasWidget or any of its ancestors are
            draggable, then register the appropriate motion callback.
        """
        # If we're already waiting for a button release, then ignore
        # this new button press.
        if (self.__canvas.bind('<ButtonRelease-1>') or
            self.__canvas.bind('<ButtonRelease-2>') or
            self.__canvas.bind('<ButtonRelease-3>')):
            return

        # Unbind motion (just in case; this shouldn't be necessary)
        self.__canvas.unbind('<Motion>')

        # Record the button press event.
        self.__press = event

        # If any ancestor is draggable, set up a motion callback.
        # (Only if they pressed button number 1)
        if event.num == 1:
            widget = self
            while widget is not None:
                if widget['draggable']:
                    widget.__start_drag(event)
                    break
                widget = widget.parent()

        # Set up the button release callback.
        self.__canvas.bind('<ButtonRelease-%d>' % event.num,
                          self.__release_cb)

    def __start_drag(self, event):
        """
        Begin dragging this object:
          - register a motion callback
          - record the drag coordinates
        """
        self.__canvas.bind('<Motion>', self.__motion_cb)
        self.__drag_x = event.x
        self.__drag_y = event.y

    def __motion_cb(self, event):
        """
        Handle a motion event:
          - move this object to the new location
          - record the new drag coordinates
        """
        self.move(event.x-self.__drag_x, event.y-self.__drag_y)
        self.__drag_x = event.x
        self.__drag_y = event.y

    def __release_cb(self, event):
        """
        Handle a release callback:
          - unregister motion & button release callbacks.
          - decide whether they clicked, dragged, or cancelled
          - call the appropriate handler.
        """
        # Unbind the button release & motion callbacks.
        self.__canvas.unbind('<ButtonRelease-%d>' % event.num)
        self.__canvas.unbind('<Motion>')

        # Is it a click or a drag?
        if (event.time - self.__press.time < 100 and
            abs(event.x-self.__press.x) + abs(event.y-self.__press.y) < 5):
            # Move it back, if we were dragging.
            if self.__draggable and event.num == 1:
                self.move(self.__press.x - self.__drag_x,
                          self.__press.y - self.__drag_y)
            self.__click(event.num)
        elif event.num == 1:
            self.__drag()

        self.__press = None

    def __drag(self):
        """
        If this ``CanvasWidget`` has a drag callback, then call it;
        otherwise, find the closest ancestor with a drag callback, and
        call it.  If no ancestors have a drag callback, do nothing.
        """
        if self.__draggable:
            if 'drag' in self.__callbacks:
                cb = self.__callbacks['drag']
                try:
                    cb(self)
                except:
                    print('Error in drag callback for ' + str(self))
        elif self.__parent is not None:
            self.__parent.__drag()

    def __click(self, button):
        """
        If this ``CanvasWidget`` has a drag callback, then call it;
        otherwise, find the closest ancestor with a click callback, and
        call it.  If no ancestors have a click callback, do nothing.
        """
        if button in self.__callbacks:
            cb = self.__callbacks[button]
            #try:
            cb(self)
            #except:
            #    print 'Error in click callback for %r' % self
            #    raise
        elif self.__parent is not None:
            self.__parent.__click(button)

    ##//////////////////////////////////////////////////////
    ##  Child/parent Handling
    ##//////////////////////////////////////////////////////

    def _add_child_widget(self, child):
        """
        Register a hierarchical child widget.  The child will be
        considered part of this canvas widget for purposes of user
        interaction.  ``_add_child_widget`` has two direct effects:
          - It sets ``child``'s parent to this canvas widget.
          - It adds ``child`` to the list of canvas widgets returned by
            the ``child_widgets`` member function.

        :param child: The new child widget.  ``child`` must not already
            have a parent.
        :type child: CanvasWidget
        """
        if not hasattr(self, '_CanvasWidget__children'): self.__children = []
        if child.__parent is not None:
            raise ValueError('%s already has a parent', child)
        child.__parent = self
        self.__children.append(child)

    def _remove_child_widget(self, child):
        """
        Remove a hierarchical child widget.  This child will no longer
        be considered part of this canvas widget for purposes of user
        interaction.  ``_add_child_widget`` has two direct effects:
          - It sets ``child``'s parent to None.
          - It removes ``child`` from the list of canvas widgets
            returned by the ``child_widgets`` member function.

        :param child: The child widget to remove.  ``child`` must be a
            child of this canvas widget.
        :type child: CanvasWidget
        """
        self.__children.remove(child)
        child.__parent = None

    ##//////////////////////////////////////////////////////
    ##  Defined by subclass
    ##//////////////////////////////////////////////////////

    def _tags(self):
        """
        :return: a list of canvas tags for all graphical elements
            managed by this canvas widget, not including graphical
            elements managed by its child widgets.
        :rtype: list of int
        """
        raise NotImplementedError()

    def _manage(self):
        """
        Arrange the child widgets of this canvas widget.  This method
        is called when the canvas widget is initially created.  It is
        also called if the user calls the ``manage`` method on this
        canvas widget or any of its ancestors.

        :rtype: None
        """
        pass

    def _update(self, child):
        """
        Update this canvas widget in response to a change in one of
        its children.

        :param child: The child that changed.
        :type child: CanvasWidget
        :rtype: None
        """
        pass

##//////////////////////////////////////////////////////
##  More basic widgets. Also extracted fomr nltk.draw.util
##//////////////////////////////////////////////////////

class TextWidget(CanvasWidget):
    """
    A canvas widget that displays a single string of text.

    Attributes:
      - ``color``: the color of the text.
      - ``font``: the font used to display the text.
      - ``justify``: justification for multi-line texts.  Valid values
        are ``left``, ``center``, and ``right``.
      - ``width``: the width of the text.  If the text is wider than
        this width, it will be line-wrapped at whitespace.
      - ``draggable``: whether the text can be dragged by the user.
    """
    def __init__(self, canvas, text, **attribs):
        """
        Create a new text widget.

        :type canvas: Tkinter.Canvas
        :param canvas: This canvas widget's canvas.
        :type text: str
        :param text: The string of text to display.
        :param attribs: The new canvas widget's attributes.
        """
        self._text = text
        self._tag = canvas.create_text(1, 1, text=text)
        CanvasWidget.__init__(self, canvas, **attribs)

    def __setitem__(self, attr, value):
        if attr in ('color', 'font', 'justify', 'width'):
            if attr == 'color': attr = 'fill'
            self.canvas().itemconfig(self._tag, {attr:value})
        else:
            CanvasWidget.__setitem__(self, attr, value)

    def __getitem__(self, attr):
        if attr == 'width':
            return int(self.canvas().itemcget(self._tag, attr))
        elif attr in ('color', 'font', 'justify'):
            if attr == 'color': attr = 'fill'
            return self.canvas().itemcget(self._tag, attr)
        else:
            return CanvasWidget.__getitem__(self, attr)

    def _tags(self): return [self._tag]

    def text(self):
        """
        :return: The text displayed by this text widget.
        :rtype: str
        """
        return self.canvas().itemcget(self._tag, 'TEXT')

    def set_text(self, text):
        """
        Change the text that is displayed by this text widget.

        :type text: str
        :param text: The string of text to display.
        :rtype: None
        """
        self.canvas().itemconfig(self._tag, text=text)
        if self.parent() is not None:
            self.parent().update(self)

    def __repr__(self):
        return '[Text: %r]' % self._text

class AbstractContainerWidget(CanvasWidget):
    """
    An abstract class for canvas widgets that contain a single child,
    such as ``BoxWidget`` and ``OvalWidget``.  Subclasses must define
    a constructor, which should create any new graphical elements and
    then call the ``AbstractCanvasContainer`` constructor.  Subclasses
    must also define the ``_update`` method and the ``_tags`` method;
    and any subclasses that define attributes should define
    ``__setitem__`` and ``__getitem__``.
    """
    def __init__(self, canvas, child, **attribs):
        """
        Create a new container widget.  This constructor should only
        be called by subclass constructors.

        :type canvas: Tkinter.Canvas
        :param canvas: This canvas widget's canvas.
        :param child: The container's child widget.  ``child`` must not
            have a parent.
        :type child: CanvasWidget
        :param attribs: The new canvas widget's attributes.
        """
        self._child = child
        self._add_child_widget(child)
        CanvasWidget.__init__(self, canvas, **attribs)

    def _manage(self):
        self._update(self._child)

    def child(self):
        """
        :return: The child widget contained by this container widget.
        :rtype: CanvasWidget
        """
        return self._child

    def set_child(self, child):
        """
        Change the child widget contained by this container widget.

        :param child: The new child widget.  ``child`` must not have a
            parent.
        :type child: CanvasWidget
        :rtype: None
        """
        self._remove_child_widget(self._child)
        self._add_child_widget(child)
        self._child = child
        self.update(child)

    def __repr__(self):
        name = self.__class__.__name__
        if name[-6:] == 'Widget': name = name[:-6]
        return '[%s: %r]' % (name, self._child)

class BoxWidget(AbstractContainerWidget):
    """
    A canvas widget that places a box around a child widget.

    Attributes:
      - ``fill``: The color used to fill the interior of the box.
      - ``outline``: The color used to draw the outline of the box.
      - ``width``: The width of the outline of the box.
      - ``margin``: The number of pixels space left between the child
        and the box.
      - ``draggable``: whether the text can be dragged by the user.
    """
    def __init__(self, canvas, child, **attribs):
        """
        Create a new box widget.

        :type canvas: Tkinter.Canvas
        :param canvas: This canvas widget's canvas.
        :param child: The child widget.  ``child`` must not have a
            parent.
        :type child: CanvasWidget
        :param attribs: The new canvas widget's attributes.
        """
        self._child = child
        self._margin = 1
        self._box = canvas.create_rectangle(1,1,1,1)
        canvas.tag_lower(self._box)
        AbstractContainerWidget.__init__(self, canvas, child, **attribs)

    def __setitem__(self, attr, value):
        if attr == 'margin': self._margin = value
        elif attr in ('outline', 'fill', 'width'):
            self.canvas().itemconfig(self._box, {attr:value})
        else:
            CanvasWidget.__setitem__(self, attr, value)

    def __getitem__(self, attr):
        if attr == 'margin': return self._margin
        elif attr == 'width':
            return float(self.canvas().itemcget(self._box, attr))
        elif attr in ('outline', 'fill', 'width'):
            return self.canvas().itemcget(self._box, attr)
        else:
            return CanvasWidget.__getitem__(self, attr)

    def _update(self, child):
        (x1, y1, x2, y2) = child.bbox()
        margin = self._margin + self['width']/2
        self.canvas().coords(self._box, x1-margin, y1-margin,
                             x2+margin, y2+margin)

    def _tags(self): return [self._box]

class OvalWidget(AbstractContainerWidget):
    """
    A canvas widget that places a oval around a child widget.

    Attributes:
      - ``fill``: The color used to fill the interior of the oval.
      - ``outline``: The color used to draw the outline of the oval.
      - ``width``: The width of the outline of the oval.
      - ``margin``: The number of pixels space left between the child
        and the oval.
      - ``draggable``: whether the text can be dragged by the user.
      - ``double``: If true, then a double-oval is drawn.
    """
    def __init__(self, canvas, child, **attribs):
        """
        Create a new oval widget.

        :type canvas: Tkinter.Canvas
        :param canvas: This canvas widget's canvas.
        :param child: The child widget.  ``child`` must not have a
            parent.
        :type child: CanvasWidget
        :param attribs: The new canvas widget's attributes.
        """
        self._child = child
        self._margin = 1
        self._oval = canvas.create_oval(1,1,1,1)
        self._circle = attribs.pop('circle', False)
        self._double = attribs.pop('double', False)
        if self._double:
            self._oval2 = canvas.create_oval(1,1,1,1)
        else:
            self._oval2 = None
        canvas.tag_lower(self._oval)
        AbstractContainerWidget.__init__(self, canvas, child, **attribs)

    def __setitem__(self, attr, value):
        c = self.canvas()
        if attr == 'margin': self._margin = value
        elif attr == 'double':
            if value==True and self._oval2 is None:
                # Copy attributes & position from self._oval.
                x1, y1, x2, y2 = c.bbox(self._oval)
                w = self['width']*2
                self._oval2 = c.create_oval(x1-w, y1-w, x2+w, y2+w,
                                outline=c.itemcget(self._oval, 'outline'),
                                width=c.itemcget(self._oval, 'width'))
                c.tag_lower(self._oval2)
            if value==False and self._oval2 is not None:
                c.delete(self._oval2)
                self._oval2 = None
        elif attr in ('outline', 'fill', 'width'):
            c.itemconfig(self._oval, {attr:value})
            if self._oval2 is not None and attr!='fill':
                c.itemconfig(self._oval2, {attr:value})
            if self._oval2 is not None and attr!='fill':
                self.canvas().itemconfig(self._oval2, {attr:value})
        else:
            CanvasWidget.__setitem__(self, attr, value)

    def __getitem__(self, attr):
        if attr == 'margin': return self._margin
        elif attr == 'double': return self._double is not None
        elif attr == 'width':
            return float(self.canvas().itemcget(self._oval, attr))
        elif attr in ('outline', 'fill', 'width'):
            return self.canvas().itemcget(self._oval, attr)
        else:
            return CanvasWidget.__getitem__(self, attr)

    # The ratio between inscribed & circumscribed ovals
    RATIO = 1.4142135623730949

    def _update(self, child):
        R = OvalWidget.RATIO
        (x1, y1, x2, y2) = child.bbox()
        margin = self._margin

        # If we're a circle, pretend our contents are square.
        if self._circle:
            dx, dy = abs(x1-x2), abs(y1-y2)
            if dx > dy:
                y = (y1+y2)/2
                y1, y2 = y-dx/2, y+dx/2
            elif dy > dx:
                x = (x1+x2)/2
                x1, x2 = x-dy/2, x+dy/2

        # Find the four corners.
        left = int(( x1*(1+R) + x2*(1-R) ) / 2)
        right = left + int((x2-x1)*R)
        top = int(( y1*(1+R) + y2*(1-R) ) / 2)
        bot = top + int((y2-y1)*R)
        self.canvas().coords(self._oval, left-margin, top-margin,
                             right+margin, bot+margin)
        if self._oval2 is not None:
            self.canvas().coords(self._oval2, left-margin+2, top-margin+2,
                                 right+margin-2, bot+margin-2)

    def _tags(self):
        if self._oval2 is None:
            return [self._oval]
        else:
            return [self._oval, self._oval2]

class ParenWidget(AbstractContainerWidget):
    """
    A canvas widget that places a pair of parenthases around a child
    widget.

    Attributes:
      - ``color``: The color used to draw the parenthases.
      - ``width``: The width of the parenthases.
      - ``draggable``: whether the text can be dragged by the user.
    """
    def __init__(self, canvas, child, **attribs):
        """
        Create a new parenthasis widget.

        :type canvas: Tkinter.Canvas
        :param canvas: This canvas widget's canvas.
        :param child: The child widget.  ``child`` must not have a
            parent.
        :type child: CanvasWidget
        :param attribs: The new canvas widget's attributes.
        """
        self._child = child
        self._oparen = canvas.create_arc(1,1,1,1, style='arc',
                                         start=90, extent=180)
        self._cparen = canvas.create_arc(1,1,1,1, style='arc',
                                         start=-90, extent=180)
        AbstractContainerWidget.__init__(self, canvas, child, **attribs)

    def __setitem__(self, attr, value):
        if attr == 'color':
            self.canvas().itemconfig(self._oparen, outline=value)
            self.canvas().itemconfig(self._cparen, outline=value)
        elif attr == 'width':
            self.canvas().itemconfig(self._oparen, width=value)
            self.canvas().itemconfig(self._cparen, width=value)
        else:
            CanvasWidget.__setitem__(self, attr, value)

    def __getitem__(self, attr):
        if attr == 'color':
            return self.canvas().itemcget(self._oparen, 'outline')
        elif attr == 'width':
            return self.canvas().itemcget(self._oparen, 'width')
        else:
            return CanvasWidget.__getitem__(self, attr)

    def _update(self, child):
        (x1, y1, x2, y2) = child.bbox()
        width = max((y2-y1)/6, 4)
        self.canvas().coords(self._oparen, x1-width, y1, x1+width, y2)
        self.canvas().coords(self._cparen, x2-width, y1, x2+width, y2)

    def _tags(self): return [self._oparen, self._cparen]


class ScrollWatcherWidget(CanvasWidget):
    """
    A special canvas widget that adjusts its ``Canvas``'s scrollregion
    to always include the bounding boxes of all of its children.  The
    scroll-watcher widget will only increase the size of the
    ``Canvas``'s scrollregion; it will never decrease it.
    """
    def __init__(self, canvas, *children, **attribs):
        """
        Create a new scroll-watcher widget.

        :type canvas: Tkinter.Canvas
        :param canvas: This canvas widget's canvas.
        :type children: list(CanvasWidget)
        :param children: The canvas widgets watched by the
            scroll-watcher.  The scroll-watcher will ensure that these
            canvas widgets are always contained in their canvas's
            scrollregion.
        :param attribs: The new canvas widget's attributes.
        """
        for child in children: self._add_child_widget(child)
        CanvasWidget.__init__(self, canvas, **attribs)

    def add_child(self, canvaswidget):
        """
        Add a new canvas widget to the scroll-watcher.  The
        scroll-watcher will ensure that the new canvas widget is
        always contained in its canvas's scrollregion.

        :param canvaswidget: The new canvas widget.
        :type canvaswidget: CanvasWidget
        :rtype: None
        """
        self._add_child_widget(canvaswidget)
        self.update(canvaswidget)

    def remove_child(self, canvaswidget):
        """
        Remove a canvas widget from the scroll-watcher.  The
        scroll-watcher will no longer ensure that the new canvas
        widget is always contained in its canvas's scrollregion.

        :param canvaswidget: The canvas widget to remove.
        :type canvaswidget: CanvasWidget
        :rtype: None
        """
        self._remove_child_widget(canvaswidget)

    def _tags(self): return []

    def _update(self, child):
        self._adjust_scrollregion()

    def _adjust_scrollregion(self):
        """
        Adjust the scrollregion of this scroll-watcher's ``Canvas`` to
        include the bounding boxes of all of its children.
        """
        bbox = self.bbox()
        canvas = self.canvas()
        scrollregion = [int(n) for n in canvas['scrollregion'].split()]
        if len(scrollregion) != 4: return
        if (bbox[0] < scrollregion[0] or bbox[1] < scrollregion[1] or
            bbox[2] > scrollregion[2] or bbox[3] > scrollregion[3]):
            scrollregion = ('%d %d %d %d' %
                            (min(bbox[0], scrollregion[0]),
                             min(bbox[1], scrollregion[1]),
                             max(bbox[2], scrollregion[2]),
                         max(bbox[3], scrollregion[3])))
            canvas['scrollregion'] = scrollregion

###################################
## CanvasFame necessary for tree displays
###################################

class CanvasFrame(object):
    """
    A ``Tkinter`` frame containing a canvas and scrollbars.
    ``CanvasFrame`` uses a ``ScrollWatcherWidget`` to ensure that all of
    the canvas widgets contained on its canvas are within its
    scrollregion.  In order for ``CanvasFrame`` to make these checks,
    all canvas widgets must be registered with ``add_widget`` when they
    are added to the canvas; and destroyed with ``destroy_widget`` when
    they are no longer needed.

    If a ``CanvasFrame`` is created with no parent, then it will create
    its own main window, including a "Done" button and a "Print"
    button.
    """
    def __init__(self, parent=None, **kw):
        """
        Create a new ``CanvasFrame``.

        :type parent: Tkinter.BaseWidget or Tkinter.Tk
        :param parent: The parent ``Tkinter`` widget.  If no parent is
            specified, then ``CanvasFrame`` will create a new main
            window.
        :param kw: Keyword arguments for the new ``Canvas``.  See the
            documentation for ``Tkinter.Canvas`` for more information.
        """
        # If no parent was given, set up a top-level window.
        if parent is None:
            self._parent = Tk()
            self._parent.title('NLTK')
            self._parent.bind('<Control-p>', lambda e: self.print_to_file())
            self._parent.bind('<Control-x>', self.destroy)
            self._parent.bind('<Control-q>', self.destroy)
        else:
            self._parent = parent

        # Create a frame for the canvas & scrollbars
        self._frame = frame = Frame(self._parent)
        self._canvas = canvas = Canvas(frame, **kw)
        xscrollbar = Scrollbar(self._frame, orient='horizontal')
        yscrollbar = Scrollbar(self._frame, orient='vertical')
        xscrollbar['command'] = canvas.xview
        yscrollbar['command'] = canvas.yview
        canvas['xscrollcommand'] = xscrollbar.set
        canvas['yscrollcommand'] = yscrollbar.set
        yscrollbar.pack(fill='y', side='right')
        xscrollbar.pack(fill='x', side='bottom')
        canvas.pack(expand=1, fill='both', side='left')

        # Set initial scroll region.
        scrollregion = '0 0 %s %s' % (canvas['width'], canvas['height'])
        canvas['scrollregion'] = scrollregion

        self._scrollwatcher = ScrollWatcherWidget(canvas)

        # If no parent was given, pack the frame, and add a menu.
        if parent is None:
            self.pack(expand=1, fill='both')
            self._init_menubar()

    def _init_menubar(self):
        menubar = Menu(self._parent)

        filemenu = Menu(menubar, tearoff=0)
        filemenu.add_command(label='Print to Postscript', underline=0,
                             command=self.print_to_file, accelerator='Ctrl-p')
        filemenu.add_command(label='Exit', underline=1,
                             command=self.destroy, accelerator='Ctrl-x')
        menubar.add_cascade(label='File', underline=0, menu=filemenu)

        self._parent.config(menu=menubar)

    def print_to_file(self, filename=None):
        """
        Print the contents of this ``CanvasFrame`` to a postscript
        file.  If no filename is given, then prompt the user for one.

        :param filename: The name of the file to print the tree to.
        :type filename: str
        :rtype: None
        """
        if filename is None:
            from tkFileDialog import asksaveasfilename
            ftypes = [('Postscript files', '.ps'),
                      ('All files', '*')]
            filename = asksaveasfilename(filetypes=ftypes,
                                         defaultextension='.ps')
            if not filename: return
        (x0, y0, w, h) = self.scrollregion()
        self._canvas.postscript(file=filename, x=x0, y=y0,
                                width=w+2, height=h+2,
                                pagewidth=w+2, # points = 1/72 inch
                                pageheight=h+2, # points = 1/72 inch
                                pagex=0, pagey=0)

    def scrollregion(self):
        """
        :return: The current scroll region for the canvas managed by
            this ``CanvasFrame``.
        :rtype: 4-tuple of int
        """
        (x1, y1, x2, y2) = self._canvas['scrollregion'].split()
        return (int(x1), int(y1), int(x2), int(y2))

    def canvas(self):
        """
        :return: The canvas managed by this ``CanvasFrame``.
        :rtype: Tkinter.Canvas
        """
        return self._canvas

    def add_widget(self, canvaswidget, x=None, y=None):
        """
        Register a canvas widget with this ``CanvasFrame``.  The
        ``CanvasFrame`` will ensure that this canvas widget is always
        within the ``Canvas``'s scrollregion.  If no coordinates are
        given for the canvas widget, then the ``CanvasFrame`` will
        attempt to find a clear area of the canvas for it.

        :type canvaswidget: CanvasWidget
        :param canvaswidget: The new canvas widget.  ``canvaswidget``
            must have been created on this ``CanvasFrame``'s canvas.
        :type x: int
        :param x: The initial x coordinate for the upper left hand
            corner of ``canvaswidget``, in the canvas's coordinate
            space.
        :type y: int
        :param y: The initial y coordinate for the upper left hand
            corner of ``canvaswidget``, in the canvas's coordinate
            space.
        """
        if x is None or y is None:
            (x, y) = self._find_room(canvaswidget, x, y)

        # Move to (x,y)
        (x1,y1,x2,y2) = canvaswidget.bbox()
        canvaswidget.move(x-x1,y-y1)

        # Register with scrollwatcher.
        self._scrollwatcher.add_child(canvaswidget)

    def _find_room(self, widget, desired_x, desired_y):
        """
        Try to find a space for a given widget.
        """
        (left, top, right, bot) = self.scrollregion()
        w = widget.width()
        h = widget.height()

        if w >= (right-left): return (0,0)
        if h >= (bot-top): return (0,0)

        # Move the widget out of the way, for now.
        (x1,y1,x2,y2) = widget.bbox()
        widget.move(left-x2-50, top-y2-50)

        if desired_x is not None:
            x = desired_x
            for y in range(top, bot-h, (bot-top-h)/10):
                if not self._canvas.find_overlapping(x-5, y-5, x+w+5, y+h+5):
                    return (x,y)

        if desired_y is not None:
            y = desired_y
            for x in range(left, right-w, (right-left-w)/10):
                if not self._canvas.find_overlapping(x-5, y-5, x+w+5, y+h+5):
                    return (x,y)

        for y in range(top, bot-h, (bot-top-h)/10):
            for x in range(left, right-w, (right-left-w)/10):
                if not self._canvas.find_overlapping(x-5, y-5, x+w+5, y+h+5):
                    return (x,y)
        return (0,0)

    def destroy_widget(self, canvaswidget):
        """
        Remove a canvas widget from this ``CanvasFrame``.  This
        deregisters the canvas widget, and destroys it.
        """
        self.remove_widget(canvaswidget)
        canvaswidget.destroy()

    def remove_widget(self, canvaswidget):
        # Deregister with scrollwatcher.
        self._scrollwatcher.remove_child(canvaswidget)

    def pack(self, cnf={}, **kw):
        """
        Pack this ``CanvasFrame``.  See the documentation for
        ``Tkinter.Pack`` for more information.
        """
        self._frame.pack(cnf, **kw)
        # Adjust to be big enough for kids?

    def destroy(self, *e):
        """
        Destroy this ``CanvasFrame``.  If this ``CanvasFrame`` created a
        top-level window, then this will close that window.
        """
        if self._parent is None: return
        self._parent.destroy()
        self._parent = None

    def mainloop(self, *args, **kwargs):
        """
        Enter the Tkinter mainloop.  This function must be called if
        this frame is created from a non-interactive program (e.g.
        from a secript); otherwise, the frame will close as soon as
        the script completes.
        """
        #if in_idle(): return #removed EA
        self._parent.mainloop(*args, **kwargs)

    
##////////////////////////////////////////////////
## Tree was extracted from nltk.tree
##////////////////////////////////////////////////

class Tree(list):
    def __init__(self, node_or_str, children=None):
        if children is None: 
            if not isinstance(node_or_str, str):
                raise TypeError("%s: Expected a node value and child list "
                                "or a single string" % type(self).__name__)
            tree = type(self).parse(node_or_str)
            list.__init__(self, tree)
            self.node = tree.node
        elif isinstance(children, str):    #changed EA (basestring = str)
            raise TypeError("%s() argument 2 should be a list, not a "
                            "string" % type(self).__name__)
        else:
            list.__init__(self, children)
            self.node = node_or_str


    def __eq__(self, other):
        if not isinstance(other, Tree): return False
        return self.node == other.node and list.__eq__(self, other)
    def __ne__(self, other):
        return not (self == other)
    def __lt__(self, other):
        if not isinstance(other, Tree): return False
        return self.node < other.node or list.__lt__(self, other)
    def __le__(self, other):
        if not isinstance(other, Tree): return False
        return self.node <= other.node or list.__le__(self, other)
    def __gt__(self, other):
        if not isinstance(other, Tree): return True
        return self.node > other.node or list.__gt__(self, other)
    def __ge__(self, other):
        if not isinstance(other, Tree): return False
        return self.node >= other.node or list.__ge__(self, other)

    #////////////////////////////////////////////////////////////
    # Disabled list operations
    #////////////////////////////////////////////////////////////

    def __mul__(self, v):
        raise TypeError('Tree does not support multiplication')
    def __rmul__(self, v):
        raise TypeError('Tree does not support multiplication')
    def __add__(self, v):
        raise TypeError('Tree does not support addition')
    def __radd__(self, v):
        raise TypeError('Tree does not support addition')

    #////////////////////////////////////////////////////////////
    # Indexing (with support for tree positions)
    #////////////////////////////////////////////////////////////

    def __getitem__(self, index):
        if isinstance(index, (int, slice)):
            return list.__getitem__(self, index)
        elif isinstance(index, (list, tuple)):
            if len(index) == 0:
                return self
            elif len(index) == 1:
                return self[index[0]]
            else:
                return self[index[0]][index[1:]]
        else:
            raise TypeError("%s indices must be integers, not %s" %
                            (type(self).__name__, type(index).__name__))

    def __setitem__(self, index, value):
        if isinstance(index, (int, slice)):
            return list.__setitem__(self, index, value)
        elif isinstance(index, (list, tuple)):
            if len(index) == 0:
                raise IndexError('The tree position () may not be '
                                 'assigned to.')
            elif len(index) == 1:
                self[index[0]] = value
            else:
                self[index[0]][index[1:]] = value
        else:
            raise TypeError("%s indices must be integers, not %s" %
                            (type(self).__name__, type(index).__name__))

    def __delitem__(self, index):
        if isinstance(index, (int, slice)):
            return list.__delitem__(self, index)
        elif isinstance(index, (list, tuple)):
            if len(index) == 0:
                raise IndexError('The tree position () may not be deleted.')
            elif len(index) == 1:
                del self[index[0]]
            else:
                del self[index[0]][index[1:]]
        else:
            raise TypeError("%s indices must be integers, not %s" %
                            (type(self).__name__, type(index).__name__))

    #////////////////////////////////////////////////////////////
    # Basic tree operations
    #////////////////////////////////////////////////////////////

    def leaves(self):
        """
        Return the leaves of the tree.

            >>> t = Tree("(S (NP (D the) (N dog)) (VP (V chased) (NP (D the) (N cat))))")
            >>> t.leaves()
            ['the', 'dog', 'chased', 'the', 'cat']

        :return: a list containing this tree's leaves.
            The order reflects the order of the
            leaves in the tree's hierarchical structure.
        :rtype: list
        """
        leaves = []
        for child in self:
            if isinstance(child, Tree):
                leaves.extend(child.leaves())
            else:
                leaves.append(child)
        return leaves

    def flatten(self):
        """
        Return a flat version of the tree, with all non-root non-terminals removed.

            >>> t = Tree("(S (NP (D the) (N dog)) (VP (V chased) (NP (D the) (N cat))))")
            >>> print t.flatten()
            (S the dog chased the cat)

        :return: a tree consisting of this tree's root connected directly to
            its leaves, omitting all intervening non-terminal nodes.
        :rtype: Tree
        """
        return Tree(self.node, self.leaves())

    def height(self):
        """
        Return the height of the tree.

            >>> t = Tree("(S (NP (D the) (N dog)) (VP (V chased) (NP (D the) (N cat))))")
            >>> t.height()
            5
            >>> print t[0,0]
            (D the)
            >>> t[0,0].height()
            2

        :return: The height of this tree.  The height of a tree
            containing no children is 1; the height of a tree
            containing only leaves is 2; and the height of any other
            tree is one plus the maximum of its children's
            heights.
        :rtype: int
        """
        max_child_height = 0
        for child in self:
            if isinstance(child, Tree):
                max_child_height = max(max_child_height, child.height())
            else:
                max_child_height = max(max_child_height, 1)
        return 1 + max_child_height

    def treepositions(self, order='preorder'):
        """
            >>> t = Tree("(S (NP (D the) (N dog)) (VP (V chased) (NP (D the) (N cat))))")
            >>> t.treepositions() # doctest: +ELLIPSIS
            [(), (0,), (0, 0), (0, 0, 0), (0, 1), (0, 1, 0), (1,), (1, 0), (1, 0, 0), ...]
            >>> for pos in t.treepositions('leaves'):
            ...     t[pos] = t[pos][::-1].upper()
            >>> print t
            (S (NP (D EHT) (N GOD)) (VP (V DESAHC) (NP (D EHT) (N TAC))))

        :param order: One of: ``preorder``, ``postorder``, ``bothorder``,
            ``leaves``.
        """
        positions = []
        if order in ('preorder', 'bothorder'): positions.append( () )
        for i, child in enumerate(self):
            if isinstance(child, Tree):
                childpos = child.treepositions(order)
                positions.extend((i,)+p for p in childpos)
            else:
                positions.append( (i,) )
        if order in ('postorder', 'bothorder'): positions.append( () )
        return positions

    def subtrees(self, filter=None):
        """
        Generate all the subtrees of this tree, optionally restricted
        to trees matching the filter function.

            >>> t = Tree("(S (NP (D the) (N dog)) (VP (V chased) (NP (D the) (N cat))))")
            >>> for s in t.subtrees(lambda t: t.height() == 2):
            ...     print s
            (D the)
            (N dog)
            (V chased)
            (D the)
            (N cat)

        :type filter: function
        :param filter: the function to filter all local trees
        """
        if not filter or filter(self):
            yield self
        for child in self:
            if isinstance(child, Tree):
                for subtree in child.subtrees(filter):
                    yield subtree

    def productions(self):
        """
        Generate the productions that correspond to the non-terminal nodes of the tree.
        For each subtree of the form (P: C1 C2 ... Cn) this produces a production of the
        form P -> C1 C2 ... Cn.

            >>> t = Tree("(S (NP (D the) (N dog)) (VP (V chased) (NP (D the) (N cat))))")
            >>> t.productions()
            [S -> NP VP, NP -> D N, D -> 'the', N -> 'dog', VP -> V NP, V -> 'chased',
            NP -> D N, D -> 'the', N -> 'cat']

        :rtype: list(Production)
        """

        if not isinstance(self.node, basestring):
            raise TypeError('Productions can only be generated from trees having node labels that are strings')

        prods = [Production(Nonterminal(self.node), _child_names(self))]
        for child in self:
            if isinstance(child, Tree):
                prods += child.productions()
        return prods

    def pos(self):
        """
        Return a sequence of pos-tagged words extracted from the tree.

            >>> t = Tree("(S (NP (D the) (N dog)) (VP (V chased) (NP (D the) (N cat))))")
            >>> t.pos()
            [('the', 'D'), ('dog', 'N'), ('chased', 'V'), ('the', 'D'), ('cat', 'N')]

        :return: a list of tuples containing leaves and pre-terminals (part-of-speech tags).
            The order reflects the order of the leaves in the tree's hierarchical structure.
        :rtype: list(tuple)
        """
        pos = []
        for child in self:
            if isinstance(child, Tree):
                pos.extend(child.pos())
            else:
                pos.append((child, self.node))
        return pos

    def leaf_treeposition(self, index):
        """
        :return: The tree position of the ``index``-th leaf in this
            tree.  I.e., if ``tp=self.leaf_treeposition(i)``, then
            ``self[tp]==self.leaves()[i]``.

        :raise IndexError: If this tree contains fewer than ``index+1``
            leaves, or if ``index<0``.
        """
        if index < 0: raise IndexError('index must be non-negative')

        stack = [(self, ())]
        while stack:
            value, treepos = stack.pop()
            if not isinstance(value, Tree):
                if index == 0: return treepos
                else: index -= 1
            else:
                for i in range(len(value)-1, -1, -1):
                    stack.append( (value[i], treepos+(i,)) )

        raise IndexError('index must be less than or equal to len(self)')

    def treeposition_spanning_leaves(self, start, end):
        """
        :return: The tree position of the lowest descendant of this
            tree that dominates ``self.leaves()[start:end]``.
        :raise ValueError: if ``end <= start``
        """
        if end <= start:
            raise ValueError('end must be greater than start')
        # Find the tree positions of the start & end leaves, and
        # take the longest common subsequence.
        start_treepos = self.leaf_treeposition(start)
        end_treepos = self.leaf_treeposition(end-1)
        # Find the first index where they mismatch:
        for i in range(len(start_treepos)):
            if i == len(end_treepos) or start_treepos[i] != end_treepos[i]:
                return start_treepos[:i]
        return start_treepos

    '''  #May not need the folowing code, depending on what I find...
    def draw(self):
        """
        Open a new window containing a graphical diagram of this tree.
        """
        from nltk.draw.tree import draw_trees
        draw_trees(self)
    '''
    
##////////////////////////////////////////////////
## Tree drawers extracted from nltk.draw.tree (dependency on nltk.tree)
##////////////////////////////////////////////////
    
class TreeSegmentWidget(CanvasWidget):
    """
    A canvas widget that displays a single segment of a hierarchical
    tree.  Each ``TreeSegmentWidget`` connects a single "node widget"
    to a sequence of zero or more "subtree widgets".  By default, the
    bottom of the node is connected to the top of each subtree by a
    single line.  However, if the ``roof`` attribute is set, then a
    single triangular "roof" will connect the node to all of its
    children.

    Attributes:
      - ``roof``: What sort of connection to draw between the node and
        its subtrees.  If ``roof`` is true, draw a single triangular
        "roof" over the subtrees.  If ``roof`` is false, draw a line
        between each subtree and the node.  Default value is false.
      - ``xspace``: The amount of horizontal space to leave between
        subtrees when managing this widget.  Default value is 10.
      - ``yspace``: The amount of space to place between the node and
        its children when managing this widget.  Default value is 15.
      - ``color``: The color of the lines connecting the node to its
        subtrees; and of the outline of the triangular roof.  Default
        value is ``'#006060'``.
      - ``fill``: The fill color for the triangular roof.  Default
        value is ``''`` (no fill).
      - ``width``: The width of the lines connecting the node to its
        subtrees; and of the outline of the triangular roof.  Default
        value is 1.
      - ``orientation``: Determines whether the tree branches downwards
        or rightwards.  Possible values are ``'horizontal'`` and
        ``'vertical'``.  The default value is ``'vertical'`` (i.e.,
        branch downwards).
      - ``draggable``: whether the widget can be dragged by the user.
    """
    def __init__(self, canvas, node, subtrees, **attribs):
        """
        :type node:
        :type subtrees: list(CanvasWidgetI)
        """
        self._node = node
        self._subtrees = subtrees

        # Attributes
        self._horizontal = 0
        self._roof = 0
        self._xspace = 10
        self._yspace = 15
        self._ordered = False

        # Create canvas objects.
        self._lines = [canvas.create_line(0,0,0,0, fill='#006060')
                       for c in subtrees]
        self._polygon = canvas.create_polygon(0,0, fill='', state='hidden',
                                              outline='#006060')

        # Register child widgets (node + subtrees)
        self._add_child_widget(node)
        for subtree in subtrees:
            self._add_child_widget(subtree)

        # Are we currently managing?
        self._managing = False

        CanvasWidget.__init__(self, canvas, **attribs)

    def __setitem__(self, attr, value):
        canvas = self.canvas()
        if attr is 'roof':
            self._roof = value
            if self._roof:
                for l in self._lines: canvas.itemconfig(l, state='hidden')
                canvas.itemconfig(self._polygon, state='normal')
            else:
                for l in self._lines: canvas.itemconfig(l, state='normal')
                canvas.itemconfig(self._polygon, state='hidden')
        elif attr == 'orientation':
            if value == 'horizontal': self._horizontal = 1
            elif value == 'vertical': self._horizontal = 0
            else:
                raise ValueError('orientation must be horizontal or vertical')
        elif attr == 'color':
            for l in self._lines: canvas.itemconfig(l, fill=value)
            canvas.itemconfig(self._polygon, outline=value)
        elif isinstance(attr, tuple) and attr[0] == 'color':
            # Set the color of an individual line.
            l = self._lines[int(attr[1])]
            canvas.itemconfig(l, fill=value)
        elif attr == 'fill':
            canvas.itemconfig(self._polygon, fill=value)
        elif attr == 'width':
            canvas.itemconfig(self._polygon, {attr:value})
            for l in self._lines: canvas.itemconfig(l, {attr:value})
        elif attr in ('xspace', 'yspace'):
            if attr == 'xspace': self._xspace = value
            elif attr == 'yspace': self._yspace = value
            self.update(self._node)
        elif attr == 'ordered':
            self._ordered = value
        else:
            CanvasWidget.__setitem__(self, attr, value)

    def __getitem__(self, attr):
        if attr == 'roof': return self._roof
        elif attr == 'width':
            return self.canvas().itemcget(self._polygon, attr)
        elif attr == 'color':
            return self.canvas().itemcget(self._polygon, 'outline')
        elif isinstance(attr, tuple) and attr[0] == 'color':
            l = self._lines[int(attr[1])]
            return self.canvas().itemcget(l, 'fill')
        elif attr == 'xspace': return self._xspace
        elif attr == 'yspace': return self._yspace
        elif attr == 'orientation':
            if self._horizontal: return 'horizontal'
            else: return 'vertical'
        elif attr == 'ordered':
            return self._ordered
        else:
            return CanvasWidget.__getitem__(self, attr)

    def node(self):
        return self._node

    def subtrees(self):
        return self._subtrees[:]

    def set_node(self, node):
        """
        Set the node to ``node``.
        """
        self._remove_child_widget(self._node)
        self._add_child_widget(node)
        self._node = node
        self.update(self._node)

    def replace_child(self, oldchild, newchild):
        """
        Replace the child ``oldchild`` with ``newchild``.
        """
        index = self._subtrees.index(oldchild)
        self._subtrees[index] = newchild
        self._remove_child_widget(oldchild)
        self._add_child_widget(newchild)
        self.update(newchild)

    def remove_child(self, child):
        index = self._subtrees.index(child)
        del self._subtrees[index]
        self._remove_child_widget(child)
        self.canvas().delete(self._lines.pop())
        self.update(self._node)

    def insert_child(self, index, child):
        self._subtrees.insert(index, child)
        self._add_child_widget(child)
        self._lines.append(canvas.create_line(0,0,0,0, fill='#006060'))
        self.update(self._node)

    # but.. lines???

    def _tags(self):
        if self._roof:
            return [self._polygon]
        else:
            return self._lines

    def _subtree_top(self, child):
        if isinstance(child, TreeSegmentWidget):
            bbox = child.node().bbox()
        else:
            bbox = child.bbox()
        if self._horizontal:
            return (bbox[0], (bbox[1]+bbox[3])/2.0)
        else:
            return ((bbox[0]+bbox[2])/2.0, bbox[1])

    def _node_bottom(self):
        bbox = self._node.bbox()
        if self._horizontal:
            return (bbox[2], (bbox[1]+bbox[3])/2.0)
        else:
            return ((bbox[0]+bbox[2])/2.0, bbox[3])

    def _update(self, child):
        if len(self._subtrees) == 0: return
        if self._node.bbox() is None: return # [XX] ???

        # Which lines need to be redrawn?
        if child is self._node: need_update = self._subtrees
        else: need_update = [child]

        if self._ordered and not self._managing:
            need_update = self._maintain_order(child)

        # Update the polygon.
        (nodex, nodey) = self._node_bottom()
        (xmin, ymin, xmax, ymax) = self._subtrees[0].bbox()
        for subtree in self._subtrees[1:]:
            bbox = subtree.bbox()
            xmin = min(xmin, bbox[0])
            ymin = min(ymin, bbox[1])
            xmax = max(xmax, bbox[2])
            ymax = max(ymax, bbox[3])

        if self._horizontal:
            self.canvas().coords(self._polygon, nodex, nodey, xmin,
                                 ymin, xmin, ymax, nodex, nodey)
        else:
            self.canvas().coords(self._polygon, nodex, nodey, xmin,
                                 ymin, xmax, ymin, nodex, nodey)

        # Redraw all lines that need it.
        for subtree in need_update:
            (nodex, nodey) = self._node_bottom()
            line = self._lines[self._subtrees.index(subtree)]
            (subtreex, subtreey) = self._subtree_top(subtree)
            self.canvas().coords(line, nodex, nodey, subtreex, subtreey)

    def _maintain_order(self, child):
        if self._horizontal:
            return self._maintain_order_horizontal(child)
        else:
            return self._maintain_order_vertical(child)

    def _maintain_order_vertical(self, child):
        (left, top, right, bot) = child.bbox()

        if child is self._node:
            # Check all the leaves
            for subtree in self._subtrees:
                (x1, y1, x2, y2) = subtree.bbox()
                if bot+self._yspace > y1:
                    subtree.move(0,bot+self._yspace-y1)

            return self._subtrees
        else:
            moved = [child]
            index = self._subtrees.index(child)

            # Check leaves to our right.
            x = right + self._xspace
            for i in range(index+1, len(self._subtrees)):
                (x1, y1, x2, y2) = self._subtrees[i].bbox()
                if x > x1:
                    self._subtrees[i].move(x-x1, 0)
                    x += x2-x1 + self._xspace
                    moved.append(self._subtrees[i])

            # Check leaves to our left.
            x = left - self._xspace
            for i in range(index-1, -1, -1):
                (x1, y1, x2, y2) = self._subtrees[i].bbox()
                if x < x2:
                    self._subtrees[i].move(x-x2, 0)
                    x -= x2-x1 + self._xspace
                    moved.append(self._subtrees[i])

            # Check the node
            (x1, y1, x2, y2) = self._node.bbox()
            if y2 > top-self._yspace:
                self._node.move(0, top-self._yspace-y2)
                moved = self._subtrees

        # Return a list of the nodes we moved
        return moved

    def _maintain_order_horizontal(self, child):
        (left, top, right, bot) = child.bbox()

        if child is self._node:
            # Check all the leaves
            for subtree in self._subtrees:
                (x1, y1, x2, y2) = subtree.bbox()
                if right+self._xspace > x1:
                    subtree.move(right+self._xspace-x1)

            return self._subtrees
        else:
            moved = [child]
            index = self._subtrees.index(child)

            # Check leaves below us.
            y = bot + self._yspace
            for i in range(index+1, len(self._subtrees)):
                (x1, y1, x2, y2) = self._subtrees[i].bbox()
                if y > y1:
                    self._subtrees[i].move(0, y-y1)
                    y += y2-y1 + self._yspace
                    moved.append(self._subtrees[i])

            # Check leaves above us
            y = top - self._yspace
            for i in range(index-1, -1, -1):
                (x1, y1, x2, y2) = self._subtrees[i].bbox()
                if y < y2:
                    self._subtrees[i].move(0, y-y2)
                    y -= y2-y1 + self._yspace
                    moved.append(self._subtrees[i])

            # Check the node
            (x1, y1, x2, y2) = self._node.bbox()
            if x2 > left-self._xspace:
                self._node.move(left-self._xspace-x2, 0)
                moved = self._subtrees

        # Return a list of the nodes we moved
        return moved

    def _manage_horizontal(self):
        (nodex, nodey) = self._node_bottom()

        # Put the subtrees in a line.
        y = 20
        for subtree in self._subtrees:
            subtree_bbox = subtree.bbox()
            dx = nodex - subtree_bbox[0] + self._xspace
            dy = y - subtree_bbox[1]
            subtree.move(dx, dy)
            y += subtree_bbox[3] - subtree_bbox[1] + self._yspace

        # Find the center of their tops.
        center = 0.0
        for subtree in self._subtrees:
            center += self._subtree_top(subtree)[1]
        center /= len(self._subtrees)

        # Center the subtrees with the node.
        for subtree in self._subtrees:
            subtree.move(0, nodey-center)

    def _manage_vertical(self):
        (nodex, nodey) = self._node_bottom()

        # Put the subtrees in a line.
        x = 0
        for subtree in self._subtrees:
            subtree_bbox = subtree.bbox()
            dy = nodey - subtree_bbox[1] + self._yspace
            dx = x - subtree_bbox[0]
            subtree.move(dx, dy)
            x += subtree_bbox[2] - subtree_bbox[0] + self._xspace

        # Find the center of their tops.
        center = 0.0
        for subtree in self._subtrees:
            center += self._subtree_top(subtree)[0]/len(self._subtrees)

        # Center the subtrees with the node.
        for subtree in self._subtrees:
            subtree.move(nodex-center, 0)

    def _manage(self):
        self._managing = True
        (nodex, nodey) = self._node_bottom()
        if len(self._subtrees) == 0: return

        if self._horizontal: self._manage_horizontal()
        else: self._manage_vertical()

        # Update lines to subtrees.
        for subtree in self._subtrees:
            self._update(subtree)

        self._managing = False

    def __repr__(self):
        return '[TreeSeg %s: %s]' % (self._node, self._subtrees)

def _tree_to_treeseg(canvas, t, make_node, make_leaf,
                     tree_attribs, node_attribs,
                     leaf_attribs, loc_attribs):
    if isinstance(t, Tree):
        node = make_node(canvas, t.node, **node_attribs)
        subtrees = [_tree_to_treeseg(canvas, child, make_node, make_leaf,
                                     tree_attribs, node_attribs,
                                     leaf_attribs, loc_attribs)
                    for child in t]
        return TreeSegmentWidget(canvas, node, subtrees, **tree_attribs)
    else:
        return make_leaf(canvas, t, **leaf_attribs)

def tree_to_treesegment(canvas, t, make_node=TextWidget,
                        make_leaf=TextWidget, **attribs):
    """
    Convert a Tree into a ``TreeSegmentWidget``.

    :param make_node: A ``CanvasWidget`` constructor or a function that
        creates ``CanvasWidgets``.  ``make_node`` is used to convert
        the Tree's nodes into ``CanvasWidgets``.  If no constructor
        is specified, then ``TextWidget`` will be used.
    :param make_leaf: A ``CanvasWidget`` constructor or a function that
        creates ``CanvasWidgets``.  ``make_leaf`` is used to convert
        the Tree's leafs into ``CanvasWidgets``.  If no constructor
        is specified, then ``TextWidget`` will be used.
    :param attribs: Attributes for the canvas widgets that make up the
        returned ``TreeSegmentWidget``.  Any attribute beginning with
        ``'tree_'`` will be passed to all ``TreeSegmentWidgets`` (with
        the ``'tree_'`` prefix removed.  Any attribute beginning with
        ``'node_'`` will be passed to all nodes.  Any attribute
        beginning with ``'leaf_'`` will be passed to all leaves.  And
        any attribute beginning with ``'loc_'`` will be passed to all
        text locations (for Trees).
    """
    # Process attribs.
    tree_attribs = {}
    node_attribs = {}
    leaf_attribs = {}
    loc_attribs = {}

    for (key, value) in attribs.items():
        if key[:5] == 'tree_': tree_attribs[key[5:]] = value
        elif key[:5] == 'node_': node_attribs[key[5:]] = value
        elif key[:5] == 'leaf_': leaf_attribs[key[5:]] = value
        elif key[:4] == 'loc_': loc_attribs[key[4:]] = value
        else: raise ValueError('Bad attribute: %s' % key)
    return _tree_to_treeseg(canvas, t, make_node, make_leaf,
                                tree_attribs, node_attribs,
                                leaf_attribs, loc_attribs)

##//////////////////////////////////////////////////////
##  Tree Widget
##//////////////////////////////////////////////////////

class TreeWidget(CanvasWidget):
    """
    A canvas widget that displays a single Tree.
    ``TreeWidget`` manages a group of ``TreeSegmentWidgets`` that are
    used to display a Tree.

    Attributes:

      - ``node_attr``: Sets the attribute ``attr`` on all of the
        node widgets for this ``TreeWidget``.
      - ``node_attr``: Sets the attribute ``attr`` on all of the
        leaf widgets for this ``TreeWidget``.
      - ``loc_attr``: Sets the attribute ``attr`` on all of the
        location widgets for this ``TreeWidget`` (if it was built from
        a Tree).  Note that a location widget is a ``TextWidget``.

      - ``xspace``: The amount of horizontal space to leave between
        subtrees when managing this widget.  Default value is 10.
      - ``yspace``: The amount of space to place between the node and
        its children when managing this widget.  Default value is 15.

      - ``line_color``: The color of the lines connecting each expanded
        node to its subtrees.
      - ``roof_color``: The color of the outline of the triangular roof
        for collapsed trees.
      - ``roof_fill``: The fill color for the triangular roof for
        collapsed trees.
      - ``width``

      - ``orientation``: Determines whether the tree branches downwards
        or rightwards.  Possible values are ``'horizontal'`` and
        ``'vertical'``.  The default value is ``'vertical'`` (i.e.,
        branch downwards).

      - ``shapeable``: whether the subtrees can be independently
        dragged by the user.  THIS property simply sets the
        ``DRAGGABLE`` property on all of the ``TreeWidget``'s tree
        segments.
      - ``draggable``: whether the widget can be dragged by the user.
    """
    def __init__(self, canvas, t, make_node=TextWidget,
                 make_leaf=TextWidget, **attribs):
        # Node & leaf canvas widget constructors
        self._make_node = make_node
        self._make_leaf = make_leaf
        self._tree = t

        # Attributes.
        self._nodeattribs = {}
        self._leafattribs = {}
        self._locattribs = {'color': '#008000'}
        self._line_color = '#008080'
        self._line_width = 1
        self._roof_color = '#008080'
        self._roof_fill = '#c0c0c0'
        self._shapeable = False
        self._xspace = 10
        self._yspace = 10
        self._orientation = 'vertical'
        self._ordered = False

        # Build trees.
        self._keys = {} # treeseg -> key
        self._expanded_trees = {}      
        self._collapsed_trees = {}      
        self._nodes = []
        self._leaves = []
        #self._locs = []
        self._make_collapsed_trees(canvas, t, ())
        self._treeseg = self._make_expanded_tree(canvas, t, ())
        self._add_child_widget(self._treeseg)

        CanvasWidget.__init__(self, canvas, **attribs)

    def expanded_tree(self, *path_to_tree):
        """
        Return the ``TreeSegmentWidget`` for the specified subtree.

        :param path_to_tree: A list of indices i1, i2, ..., in, where
            the desired widget is the widget corresponding to
            ``tree.children()[i1].children()[i2]....children()[in]``.
            For the root, the path is ``()``.
        """
        return self._expanded_trees[path_to_tree]

    def collapsed_tree(self, *path_to_tree):
        """
        Return the ``TreeSegmentWidget`` for the specified subtree.

        :param path_to_tree: A list of indices i1, i2, ..., in, where
            the desired widget is the widget corresponding to
            ``tree.children()[i1].children()[i2]....children()[in]``.
            For the root, the path is ``()``.
        """
        return self._collapsed_trees[path_to_tree]

    def bind_click_trees(self, callback, button=1):
        """
        Add a binding to all tree segments.
        """
        for tseg in self._expanded_trees.values():
            tseg.bind_click(callback, button)
        for tseg in self._collapsed_trees.values():
            tseg.bind_click(callback, button)

    def bind_drag_trees(self, callback, button=1):
        """
        Add a binding to all tree segments.
        """
        for tseg in self._expanded_trees.values():
            tseg.bind_drag(callback, button)
        for tseg in self._collapsed_trees.values():
            tseg.bind_drag(callback, button)

    def bind_click_leaves(self, callback, button=1):
        """
        Add a binding to all leaves.
        """
        for leaf in self._leaves: leaf.bind_click(callback, button)
        for leaf in self._leaves: leaf.bind_click(callback, button)

    def bind_drag_leaves(self, callback, button=1):
        """
        Add a binding to all leaves.
        """
        for leaf in self._leaves: leaf.bind_drag(callback, button)
        for leaf in self._leaves: leaf.bind_drag(callback, button)

    def bind_click_nodes(self, callback, button=1):
        """
        Add a binding to all nodes.
        """
        for node in self._nodes: node.bind_click(callback, button)
        for node in self._nodes: node.bind_click(callback, button)

    def bind_drag_nodes(self, callback, button=1):
        """
        Add a binding to all nodes.
        """
        for node in self._nodes: node.bind_drag(callback, button)
        for node in self._nodes: node.bind_drag(callback, button)

    def _make_collapsed_trees(self, canvas, t, key):
        if not isinstance(t, Tree): return
        make_node = self._make_node
        make_leaf = self._make_leaf

        node = make_node(canvas, t.node, **self._nodeattribs)
        self._nodes.append(node)
        leaves = [make_leaf(canvas, l, **self._leafattribs)
                  for l in t.leaves()]
        self._leaves += leaves
        treeseg = TreeSegmentWidget(canvas, node, leaves, roof=1,
                                    color=self._roof_color,
                                    fill=self._roof_fill,
                                    width=self._line_width)

        self._collapsed_trees[key] = treeseg
        self._keys[treeseg] = key
        #self._add_child_widget(treeseg)
        treeseg.hide()

        # Build trees for children.
        for i in range(len(t)):
            child = t[i]
            self._make_collapsed_trees(canvas, child, key + (i,))

    def _make_expanded_tree(self, canvas, t, key):
        make_node = self._make_node
        make_leaf = self._make_leaf

        if isinstance(t, Tree):
            node = make_node(canvas, t.node, **self._nodeattribs)
            self._nodes.append(node)
            children = t
            subtrees = [self._make_expanded_tree(canvas, children[i], key+(i,))
                        for i in range(len(children))]
            treeseg = TreeSegmentWidget(canvas, node, subtrees,
                                        color=self._line_color,
                                        width=self._line_width)
            self._expanded_trees[key] = treeseg
            self._keys[treeseg] = key
            return treeseg
        else:
            leaf = make_leaf(canvas, t, **self._leafattribs)
            self._leaves.append(leaf)
            return leaf

    def __setitem__(self, attr, value):
        if attr[:5] == 'node_':
            for node in self._nodes: node[attr[5:]] = value
        elif attr[:5] == 'leaf_':
            for leaf in self._leaves: leaf[attr[5:]] = value
        elif attr == 'line_color':
            self._line_color = value
            for tseg in self._expanded_trees.values(): tseg['color'] = value
        elif attr == 'line_width':
            self._line_width = value
            for tseg in self._expanded_trees.values(): tseg['width'] = value
            for tseg in self._collapsed_trees.values(): tseg['width'] = value
        elif attr == 'roof_color':
            self._roof_color = value
            for tseg in self._collapsed_trees.values(): tseg['color'] = value
        elif attr == 'roof_fill':
            self._roof_fill = value
            for tseg in self._collapsed_trees.values(): tseg['fill'] = value
        elif attr == 'shapeable':
            self._shapeable = value
            for tseg in self._expanded_trees.values():
                tseg['draggable'] = value
            for tseg in self._collapsed_trees.values():
                tseg['draggable'] = value
            for leaf in self._leaves: leaf['draggable'] = value
        elif attr == 'xspace':
            self._xspace = value
            for tseg in self._expanded_trees.values():
                tseg['xspace'] = value
            for tseg in self._collapsed_trees.values():
                tseg['xspace'] = value
            self.manage()
        elif attr == 'yspace':
            self._yspace = value
            for tseg in self._expanded_trees.values():
                tseg['yspace'] = value
            for tseg in self._collapsed_trees.values():
                tseg['yspace'] = value
            self.manage()
        elif attr == 'orientation':
            self._orientation = value
            for tseg in self._expanded_trees.values():
                tseg['orientation'] = value
            for tseg in self._collapsed_trees.values():
                tseg['orientation'] = value
            self.manage()
        elif attr == 'ordered':
            self._ordered = value
            for tseg in self._expanded_trees.values():
                tseg['ordered'] = value
            for tseg in self._collapsed_trees.values():
                tseg['ordered'] = value
        else: CanvasWidget.__setitem__(self, attr, value)

    def __getitem__(self, attr):
        if attr[:5] == 'node_':
            return self._nodeattribs.get(attr[5:], None)
        elif attr[:5] == 'leaf_':
            return self._leafattribs.get(attr[5:], None)
        elif attr[:4] == 'loc_':
            return self._locattribs.get(attr[4:], None)
        elif attr == 'line_color': return self._line_color
        elif attr == 'line_width': return self._line_width
        elif attr == 'roof_color': return self._roof_color
        elif attr == 'roof_fill': return self._roof_fill
        elif attr == 'shapeable': return self._shapeable
        elif attr == 'xspace': return self._xspace
        elif attr == 'yspace': return self._yspace
        elif attr == 'orientation': return self._orientation
        else: return CanvasWidget.__getitem__(self, attr)

    def _tags(self): return []

    def _manage(self):
        segs = []                                #modified EA
        for n in self._expanded_trees.values():
            segs.append(n)
        for n in self._collapsed_trees.values():
            segs.append(n)    
        #segs = self._expanded_trees.values() + self._collapsed_trees.values()
        for tseg in segs:
            if tseg.hidden():
                tseg.show()
                tseg.manage()
                tseg.hide()

    def toggle_collapsed(self, treeseg):
        """
        Collapse/expand a tree.
        """
        old_treeseg = treeseg
        if old_treeseg['roof']:
            new_treeseg = self._expanded_trees[self._keys[old_treeseg]]
        else:
            new_treeseg = self._collapsed_trees[self._keys[old_treeseg]]

        # Replace the old tree with the new tree.
        if old_treeseg.parent() is self:
            self._remove_child_widget(old_treeseg)
            self._add_child_widget(new_treeseg)
            self._treeseg = new_treeseg
        else:
            old_treeseg.parent().replace_child(old_treeseg, new_treeseg)

        # Move the new tree to where the old tree was.  Show it first,
        # so we can find its bounding box.
        new_treeseg.show()
        (newx, newy) = new_treeseg.node().bbox()[:2]
        (oldx, oldy) = old_treeseg.node().bbox()[:2]
        new_treeseg.move(oldx-newx, oldy-newy)

        # Hide the old tree
        old_treeseg.hide()

        # We could do parent.manage() here instead, if we wanted.
        new_treeseg.parent().update(new_treeseg)


##////////////////////////////////////////////////
## TreeView was extracted from nltk.draw.tree (dependency on nltk.tree)
##////////////////////////////////////////////////

class TreeView(object):
    def __init__(self, *trees):
        from math import sqrt, ceil

        self._trees = trees

        self._top = Tk()
        self._top.title('NLTK')
        self._top.bind('<Control-x>', self.destroy)
        self._top.bind('<Control-q>', self.destroy)

        cf = self._cframe = CanvasFrame(self._top)
        self._top.bind('<Control-p>', self._cframe.print_to_file)

        # Size is variable.
        self._size = IntVar(self._top)
        self._size.set(12)
        bold = ('helvetica', -self._size.get(), 'bold')
        helv = ('helvetica', -self._size.get())

        # Lay the trees out in a square.
        self._width = int(ceil(sqrt(len(trees))))
        self._widgets = []
        for i in range(len(trees)):
            widget = TreeWidget(cf.canvas(), trees[i], node_font=bold,
                                leaf_color='#008040', node_color='#004080',
                                roof_color='#004040', roof_fill='white',
                                line_color='#004040', draggable=1,
                                leaf_font=helv)
            widget.bind_click_trees(widget.toggle_collapsed)
            self._widgets.append(widget)
            cf.add_widget(widget, 0, 0)

        self._layout()
        self._cframe.pack(expand=1, fill='both')
        self._init_menubar()
        self.mainloop()

    def _layout(self):
        i = x = y = ymax = 0
        width = self._width
        for i in range(len(self._widgets)):
            widget = self._widgets[i]
            (oldx, oldy) = widget.bbox()[:2]
            if i % width == 0:
                y = ymax
                x = 0
            widget.move(x-oldx, y-oldy)
            x = widget.bbox()[2] + 10
            ymax = max(ymax, widget.bbox()[3] + 10)

    def _init_menubar(self):
        menubar = Menu(self._top)

        filemenu = Menu(menubar, tearoff=0)
        filemenu.add_command(label='Print to Postscript', underline=0,
                             command=self._cframe.print_to_file,
                             accelerator='Ctrl-p')
        filemenu.add_command(label='Exit', underline=1,
                             command=self.destroy, accelerator='Ctrl-x')
        menubar.add_cascade(label='File', underline=0, menu=filemenu)

        zoommenu = Menu(menubar, tearoff=0)
        zoommenu.add_radiobutton(label='Tiny', variable=self._size,
                                 underline=0, value=10, command=self.resize)
        zoommenu.add_radiobutton(label='Small', variable=self._size,
                                 underline=0, value=12, command=self.resize)
        zoommenu.add_radiobutton(label='Medium', variable=self._size,
                                 underline=0, value=14, command=self.resize)
        zoommenu.add_radiobutton(label='Large', variable=self._size,
                                 underline=0, value=28, command=self.resize)
        zoommenu.add_radiobutton(label='Huge', variable=self._size,
                                 underline=0, value=50, command=self.resize)
        menubar.add_cascade(label='Zoom', underline=0, menu=zoommenu)

        self._top.config(menu=menubar)

    def resize(self, *e):
        bold = ('helvetica', -self._size.get(), 'bold')
        helv = ('helvetica', -self._size.get())
        xspace = self._size.get()
        yspace = self._size.get()
        for widget in self._widgets:
            widget['node_font'] = bold
            widget['leaf_font'] = helv
            widget['xspace'] = xspace
            widget['yspace'] = yspace
            if self._size.get() < 20: widget['line_width'] = 1
            elif self._size.get() < 30: widget['line_width'] = 2
            else: widget['line_width'] = 3
        self._layout()

    def destroy(self, *e):
        if self._top is None: return
        self._top.destroy()
        self._top = None

    def mainloop(self, *args, **kwargs):
        """
        Enter the Tkinter mainloop.  This function must be called if
        this demo is created from a non-interactive program (e.g.
        from a script); otherwise, the demo will close as soon as
        the script completes.
        """
        #if in_idle(): return  #removed, EA
        self._top.mainloop(*args, **kwargs)
