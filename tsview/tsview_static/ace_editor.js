function setConfig(editor, jsonCfg) {
    cfg = JSON.parse(jsonCfg);
    var session = editor.getSession();

    editor.setTheme(cfg.theme);
    session.setMode(cfg.mode);
    editor.setFontSize(cfg.fontSize);

    return editor;
}

function setPayload(editor, jsonPayload) {
    payload = JSON.parse(jsonPayload);
    var session = editor.getSession();

    session.setValue(payload.code);
    session.setAnnotations(payload.annotations);

    xs = payload.annotations;
    if (xs) {
        editor.gotoLine(xs[0].row + 1, xs[0].column, true);
    };

    return editor;
}

class AceReadOnly extends HTMLElement {
    constructor() {
        self = super();
        self.editor = null;
        return self;
    }
    connectedCallback() {
        var pre = document.createElement('pre');
        this.appendChild(pre);
        var editor = ace.edit(pre);

        setConfig(editor, this.getAttribute('cfg'));
        setPayload(editor, this.getAttribute('payload'));
        editor.setReadOnly(true);

        this.editor = editor;
    }
    static get observedAttributes() {
        return ['cfg', 'payload'];
    }
    attributeChangedCallback(name, oldVal, newVal) {
        var editor = this.editor;
        if(!editor) {
            return false;
        }
        switch(name){
            case 'cfg':
                setConfig(editor, newVal);
                break;
            case 'payload':
                setPayload(editor, newVal);
                break;
        }
    }
}
window.customElements.define('ace-readonly', AceReadOnly);

class AceEditor extends AceReadOnly {
    constructor() {
        super();
    }
    connectedCallback() {
        super.connectedCallback();
        var editor = this.editor;

        editor.setReadOnly(false);
        editor.getSession().on('change', (event) => {
            this.dispatchEvent(new CustomEvent("onChange", {
                detail: {value : editor.getValue()}
           }))
        }).bind(this);
    }
    static get observedAttributes() {
        return ['cfg', 'reload'];
    }
    attributeChangedCallback(name, oldVal, newVal) {
        super.attributeChangedCallback(name, oldVal, newVal);
        var editor = this.editor;
        if(!editor) {
            return false;
        }
        if (name=='reload' && newVal!='') {
            editor.session.setValue(newVal);
        }
    }
}
window.customElements.define('ace-editor', AceEditor);
