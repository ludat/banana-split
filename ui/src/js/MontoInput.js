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
    static observedAttributes = ["raw-value", "placeholder", "required", "input-id", "class", "decimal-places", "label"];

    constructor() {
        super();
        this._autoNumeric = null;
        this._input = null;
        this._label = null;
    }

    // `Html.Attributes.placeholder` y `required` de Elm ponen propiedades, no
    // atributos: estas las reflejan en el atributo, que es lo que observamos.
    get placeholder() {
        return this.getAttribute("placeholder") ?? "";
    }

    set placeholder(v) {
        this.setAttribute("placeholder", v);
    }

    get required() {
        return this.hasAttribute("required");
    }

    set required(v) {
        this.toggleAttribute("required", Boolean(v));
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
        this._syncLabel();

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
        this._syncLabel();
    }

    // Con `label`, el label va adentro del elemento y después del input: con
    // la clase `form-floating` en el elemento queda la estructura que esperan
    // los floating labels de Bootstrap (`.form-floating > .form-control ~ label`).
    // Si el label lo pusiera Elm afuera, el input no sería hijo directo.
    _syncLabel() {
        // Hasta que el input no esté adentro no hay que crear el label: tiene
        // que quedar después (`~ label`), y connectedCallback lo vuelve a llamar.
        if (this._input?.parentNode !== this) return;
        const texto = this.getAttribute("label");
        if (texto === null) {
            this._label?.remove();
            this._label = null;
            return;
        }
        if (!this._label) {
            this._label = document.createElement("label");
            this.appendChild(this._label);
        }
        this._label.htmlFor = this._input.id;
        this._label.textContent = texto;
        if (this.hasAttribute("required")) {
            const marca = document.createElement("span");
            marca.className = "text-danger ms-1";
            marca.textContent = "*";
            this._label.appendChild(marca);
        }
    }
}

customElements.define("monto-input", MontoInputElement);
