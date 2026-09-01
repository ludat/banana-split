import AutoNumeric from "autonumeric";

const AUTONUMERIC_OPTIONS = {
    decimalCharacter: ",",
    digitGroupSeparator: ".",
    allowDecimalPadding: true,
    unformatOnFocus: false,
    unformatOnHover: false,
    modifyValueOnUpDownArrow: true,
    modifyValueOnWheel: true,
    emptyInputBehavior: "null",
    eventBubbles: false,
    eventIsCancelable: true,
    formulaMode: true
};

const DECIMAL_PLACES_POR_DEFECTO = 2;

class MontoInputElement extends HTMLElement {
    static observedAttributes = ["raw-value", "placeholder", "required", "input-id", "class", "decimal-places"];

    constructor() {
        super();
        this._autoNumeric = null;
        this._input = null;
    }

    connectedCallback() {
        if (this._input) return;

        this._input = document.createElement("input");
        this._input.type = "text";
        this._input.inputMode = "decimal";
        this._input.className = this._computeInputClass();
        this._syncNonValueAttrs();
        this._input.value = AutoNumeric.format(this.getAttribute("raw-value"), this._options());
        this.appendChild(this._input);

        this._autoNumeric = new AutoNumeric(this._input, this._options());

        this._input.addEventListener("autoNumeric:rawValueModified", (e) => {
            if (Number(this._autoNumeric.getNumericString()) === Number(this.getAttribute("raw-value"))) return;
            this.dispatchEvent(new CustomEvent(e.type, { detail: e.detail }));
        });
    }

    attributeChangedCallback(name, _old, newVal) {
        if (name === "raw-value") {
            this._setFromDisplay(newVal);
        } else if (name === "class") {
            if (this._input) this._input.className = this._computeInputClass();
        } else if (name === "decimal-places") {
            if (this._autoNumeric) this._autoNumeric.update(this._options());
        } else {
            this._syncNonValueAttrs();
        }
    }

    _options() {
        const attr = this.getAttribute("decimal-places");
        const decimales = attr === null ? NaN : Number(attr);
        return {
            ...AUTONUMERIC_OPTIONS,
            decimalPlaces: Number.isInteger(decimales) && decimales >= 0
                ? decimales
                : DECIMAL_PLACES_POR_DEFECTO
        };
    }

    focus(options) {
        if (this._input) this._input.focus(options);
        else super.focus(options);
    }

    _computeInputClass() {
        return this.classList.contains("is-invalid")
            ? "form-control is-invalid"
            : "form-control";
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
        const inputId = this.getAttribute("input-id");
        if (inputId !== null) this._input.id = inputId;
    }
}

customElements.define("monto-input", MontoInputElement);
