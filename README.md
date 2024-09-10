# TextContainer
### A TMS Web Core component to store multiline text in multiple formats

This component provides a place to store lengthy multiline text strings. The stored text can be interpreted as YAML, JSON or INI.

The component comes in handy in particular for multiline text, that otherwise would be written directly into the code using that clumsy multiline string constant '...'+ notation .

Typically, text stored in such a component at design time contains some kind of configuration data. These parameters are often formatted as JSON, YAML or INI text.

The component offers properties and functions to handle and convert between those formats. In addition, a TJSValue/Object/Array can be created directly from the component.

Special design time feature:
----------------------------

The preferred method to invoke the text editor dialog at design time is to just doubleclick or rightclick the component icon. Once opened, the user may resize the edit dialog freely to have a better overview of the text entered. When closing the window, the dimensions of the resized editor dialog will be stored persistently and next time the text editor is opened again, the previous dimensions are restored.
