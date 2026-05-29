import AutoNumeric from "autonumeric";

const AUTONUMERIC_OPTIONS = {
    decimalCharacter: ",",
    digitGroupSeparator: ".",
    decimalPlaces: 2,
    allowDecimalPadding: true,
    unformatOnFocus: false,
    unformatOnHover: false,
    modifyValueOnUpDownArrow: true,
    modifyValueOnWheel: true,
    emptyInputBehavior: "null",
    eventBubbles: true,
    eventIsCancelable: true,
    formulaMode: true
};

class MontoInputElement extends HTMLElement {
    static observedAttributes = ["raw-value", "placeholder", "required", "value-state"];

    constructor() {
        super();
        this.attachShadow({ mode: "open" });
        this._autoNumeric = null;
        this._input = null;
    }

    connectedCallback() {
        this.shadowRoot.innerHTML = `
          <style>
            :host { display: inline-flex; width: 100%; }
            input {
              all: unset;
              width: 100%;
              box-sizing: border-box;
              height: var(--_ui5_input_height, 2.25rem);
              padding: 0 var(--_ui5_input_padding_right, 0.6875rem) 0 var(--_ui5_input_padding_left, 0.6875rem);
              border: var(--_ui5_input_border, 0.0625rem solid var(--sapField_BorderColor));
              border-radius: var(--_ui5_input_border_radius, 0.25rem);
              background: var(--sapField_Background, #fff);
              color: var(--sapTextColor, #32363a);
              font-family: var(--sapFontFamily, "72");
              font-size: var(--sapFontSize, 0.875rem);
              outline: none;
            }
            input:focus {
              border-color: var(--sapField_Focus_BorderColor, #0064d9);
              box-shadow: inset 0 0 0 var(--_ui5_input_focus_outline_width, 0.0625rem) var(--sapField_Focus_BorderColor, #0064d9);
            }
            :host([value-state="Negative"]) input {
              border-color: var(--sapField_InvalidColor, #aa0808);
            }
          </style>
          <input type="text" inputmode="decimal"/>
        `;

        this._input = this.shadowRoot.querySelector("input");
        this._syncNonValueAttrs();

        this._input.value = AutoNumeric.format(this.getAttribute("raw-value"), AUTONUMERIC_OPTIONS);

        this._autoNumeric = new AutoNumeric(this._input, AUTONUMERIC_OPTIONS);

        this._input.addEventListener("autoNumeric:rawValueModified", (e) => {
            // Ignore event if the raw-value is already the current value
            if(Number(this._autoNumeric.getNumericString()) === Number(this.getAttribute('raw-value'))) return;

            this.dispatchEvent(new CustomEvent(e.type, e));
        });
    }

    attributeChangedCallback(name, _old, newVal) {
        if (name === "raw-value") {
            this._setFromDisplay(newVal);
        } else {
            this._syncNonValueAttrs();
        }
    }

    _setFromDisplay(s) {
        if (!this._autoNumeric) return;
        if (s !== this._autoNumeric.getNumericString()) {
            this._autoNumeric.set(s);
        }
    }

    _syncNonValueAttrs() {
        if (!this._input) return;
        this._input.placeholder = this.getAttribute("placeholder") ?? "";
        this._input.required = this.hasAttribute("required");
    }
}

customElements.define("monto-input", MontoInputElement);
