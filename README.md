# TextContainer
### A TMS Web Core component for saving multi-line text in various formats

This component offers the possibility to save long multi-line text strings. The saved text can be interpreted as YAML, JSON or INI.

The component is particularly useful for multi-line texts that would otherwise have to be written directly into the code using the cumbersome multi-line string constant '...'+ notation.

Usually, the text stored in such a component at design time contains some kind of configuration data. These parameters are often formatted as JSON, YAML or INI text.

The component provides properties and functions for processing and converting between these formats. In addition, a TJSValue/Object/Array can be created directly from the component.

The package comes with 2 library units that can also be useful for other purposes. One unit handles the INI data format and the other handles and converts the JSON/YAML data format. Since TMS Web Core does not natively support YAML and INI processing, these units may be useful.

Special function for design time:
----------------------------

The preferred method to open the text editor dialog at design time is to simply double-click or right-click on the component icon. Once opened, the user can resize the edit dialog to get a better view of the text entered. When the window is closed, the dimensions of the resized editor dialog are saved permanently and the next time the text editor is opened, the previous dimensions are restored.
