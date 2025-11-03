(() => {
    const initialProject = JSON.parse(document.getElementById('initial-project').textContent);
    const initialQuota = JSON.parse(document.getElementById('initial-quota').textContent);
    const csrfToken = document.documentElement.dataset.csrf;
    let lastSavedUtc = document.documentElement.dataset.lastSaved;

    const TokenService = {
        NONE: 0,
        CONVERT: 64,
        SOLVE: 256,
    };

    const state = {
        project: normalizeProject(initialProject),
        quota: initialQuota,
        conflicts: [],
        dirty: false,
        solvingAt: null,
        overQuota: initialQuota.dailySpent >= initialQuota.dailyBudget,
    };

    const elements = {
        projectName: document.getElementById('project-name'),
        inputsList: document.getElementById('inputs-list'),
        outputsList: document.getElementById('outputs-list'),
        conflictsList: document.getElementById('conflicts-list'),
        quotaIndicator: document.getElementById('quota-indicator'),
        dirtyIndicator: document.getElementById('dirty-indicator'),
        addInput: document.getElementById('btn-add-input'),
        solveButton: document.getElementById('btn-solve'),
        saveButton: document.getElementById('btn-save-project'),
        newProjectButton: document.getElementById('btn-new-project'),
        openProjectButton: document.getElementById('btn-open-project'),
        projectsList: document.getElementById('projects-list'),
        solveTimestamp: document.getElementById('solve-timestamp'),
        toastElement: document.getElementById('app-toast'),
        toastMessage: document.getElementById('toast-message'),
    };

    const bootstrapToast = new bootstrap.Toast(elements.toastElement);
    const openProjectModal = new bootstrap.Modal(document.getElementById('openProjectModal'));

    elements.projectName.addEventListener('input', () => {
        state.project.name = elements.projectName.textContent.trim();
        markDirty();
    });

    elements.projectName.addEventListener('keydown', event => {
        if (event.key === 'Enter') {
            event.preventDefault();
            elements.projectName.blur();
        }
    });

    renderAll();

    elements.addInput.addEventListener('click', () => {
        state.project.inputs.push({
            id: null,
            name: `Input ${state.project.inputs.length + 1}`,
            text: '',
            order: state.project.inputs.length,
            output: null,
        });
        markDirty();
        renderAll();
    });

    elements.solveButton.addEventListener('click', async () => {
        if (!canRunActions()) {
            return;
        }

        try {
            const response = await apiPost(`/api/projects/${state.project.id}/solve`, null, TokenService.SOLVE);
            state.conflicts = response.conflicts;
            state.solvingAt = new Date().toISOString();
            renderConflicts();
            renderSolveStatus();
        } catch (error) {
            handleError(error);
        }
    });

    elements.saveButton.addEventListener('click', async () => {
        await saveProject();
    });

    elements.newProjectButton.addEventListener('click', async () => {
        const name = prompt('New project name');
        if (!name) {
            return;
        }

        try {
            const result = await apiPost('/api/projects', { name }, TokenService.NONE);
            state.project = normalizeProject(result);
            state.conflicts = [];
            state.dirty = false;
            state.solvingAt = null;
            lastSavedUtc = state.project.updatedUtc;
            updateAfterSave();
        } catch (error) {
            handleError(error);
        }
    });

    elements.openProjectButton.addEventListener('click', async () => {
        await loadProjectList();
        openProjectModal.show();
    });

    document.body.addEventListener('htmx:afterRequest', event => {
        if (event.target.id === 'quota-fetcher') {
            try {
                const data = JSON.parse(event.detail.xhr.responseText);
                updateQuota(data);
            } catch (err) {
                console.error('Failed to parse quota response', err);
            }
        }
    });

    function normalizeProject(project) {
        const inputs = Array.isArray(project.inputs) ? project.inputs : [];
        project.inputs = inputs
            .map((input, index) => ({
                id: input.id ?? null,
                name: input.name ?? `Input ${index + 1}`,
                text: input.text ?? '',
                order: input.order ?? index,
                output: input.output ?? null,
            }))
            .sort((a, b) => a.order - b.order);
        return project;
    }

    function renderAll() {
        renderProjectName();
        renderInputs();
        renderOutputs();
        renderConflicts();
        renderQuota();
        renderSolveStatus();
        updateButtonStates();
    }

    function renderProjectName() {
        elements.projectName.textContent = state.project.name;
    }

    function renderInputs() {
        elements.inputsList.innerHTML = '';
        state.project.inputs.forEach((input, index) => {
            input.order = index;
            const wrapper = document.createElement('div');
            wrapper.className = 'border rounded p-2';

            const header = document.createElement('div');
            header.className = 'd-flex justify-content-between align-items-center gap-2';

            const nameInput = document.createElement('input');
            nameInput.type = 'text';
            nameInput.value = input.name;
            nameInput.className = 'form-control form-control-sm input-name';
            nameInput.addEventListener('input', e => {
                input.name = e.target.value;
                markDirty();
            });

            const buttonGroup = document.createElement('div');
            buttonGroup.className = 'btn-group btn-group-sm';

            const convertButton = document.createElement('button');
            convertButton.type = 'button';
            convertButton.className = 'btn btn-outline-primary';
            convertButton.textContent = 'Convert';
            convertButton.disabled = !canRunActions() || input.id === null;
            convertButton.addEventListener('click', () => convertInput(input));

            const deleteButton = document.createElement('button');
            deleteButton.type = 'button';
            deleteButton.className = 'btn btn-outline-danger';
            deleteButton.textContent = 'Delete';
            deleteButton.addEventListener('click', () => {
                const idx = state.project.inputs.indexOf(input);
                if (idx >= 0) {
                    state.project.inputs.splice(idx, 1);
                    markDirty();
                    renderAll();
                }
            });

            buttonGroup.append(convertButton, deleteButton);
            header.append(nameInput, buttonGroup);

            const textArea = document.createElement('textarea');
            textArea.className = 'form-control mt-2';
            textArea.value = input.text;
            textArea.addEventListener('input', e => {
                input.text = e.target.value;
                markDirty();
            });

            wrapper.append(header, textArea);
            elements.inputsList.appendChild(wrapper);
        });
    }

    function renderOutputs() {
        elements.outputsList.innerHTML = '';
        state.project.inputs.forEach(input => {
            const wrapper = document.createElement('div');
            wrapper.className = 'border rounded p-2';

            const title = document.createElement('div');
            title.className = 'd-flex justify-content-between align-items-center gap-2';
            title.innerHTML = `<strong>${escapeHtml(input.name)}</strong>`;

            const textArea = document.createElement('textarea');
            textArea.className = 'form-control mt-2';
            textArea.value = input.output?.text ?? '';
            textArea.addEventListener('input', e => {
                if (!input.output) {
                    input.output = { id: null, text: '', convertedUtc: null };
                }
                input.output.text = e.target.value;
            });

            const timestamp = document.createElement('div');
            timestamp.className = 'output-timestamp mt-2';
            timestamp.textContent = input.output?.convertedUtc
                ? `Converted at ${formatTime(input.output.convertedUtc)}`
                : 'Not converted yet';

            wrapper.append(title, textArea, timestamp);
            elements.outputsList.appendChild(wrapper);
        });
    }

    function renderConflicts() {
        elements.conflictsList.innerHTML = '';
        if (!state.conflicts || state.conflicts.length === 0) {
            const empty = document.createElement('div');
            empty.className = 'text-muted';
            empty.textContent = 'No conflicts yet';
            elements.conflictsList.appendChild(empty);
            return;
        }

        state.conflicts.forEach(conflict => {
            const badge = document.createElement('div');
            const kindClass = conflict.kind === 'Info' ? 'badge-info' : conflict.kind === 'MissingOutput' ? 'badge-warning' : 'badge-error';
            badge.className = `badge ${kindClass} text-wrap p-2`;
            badge.textContent = conflict.file ? `${conflict.file}: ${conflict.message}` : conflict.message;
            elements.conflictsList.appendChild(badge);
        });
    }

    function renderQuota() {
        const remaining = Math.max(0, state.quota.dailyBudget - state.quota.dailySpent);
        elements.quotaIndicator.textContent = `Quota: ${state.quota.dailySpent}/${state.quota.dailyBudget} tokens (remaining ${remaining})`;
        state.overQuota = state.quota.dailySpent >= state.quota.dailyBudget;
        updateButtonStates();
    }

    function renderSolveStatus() {
        if (state.solvingAt) {
            elements.solveTimestamp.textContent = `Last solve: ${formatTime(state.solvingAt)}`;
        } else {
            elements.solveTimestamp.textContent = '';
        }
    }

    async function convertInput(input) {
        if (!canRunActions() || input.id === null) {
            return;
        }

        try {
            const response = await apiPost(`/api/inputs/${input.id}/convert`, null, TokenService.CONVERT);
            input.output = {
                id: input.output?.id ?? response.inputId,
                text: response.output,
                convertedUtc: response.convertedUtc,
            };
            markClean();
            renderOutputs();
            renderQuota();
        } catch (error) {
            handleError(error);
        }
    }

    function markDirty() {
        state.dirty = true;
        elements.dirtyIndicator.classList.remove('d-none');
        updateButtonStates();
    }

    function markClean() {
        state.dirty = false;
        elements.dirtyIndicator.classList.add('d-none');
        updateButtonStates();
    }

    function updateButtonStates() {
        const canSave = state.dirty;
        elements.saveButton.disabled = !canSave;
        elements.solveButton.disabled = state.dirty || state.overQuota;
        document.querySelectorAll('#inputs-list button.btn-outline-primary').forEach(btn => {
            btn.disabled = state.dirty || state.overQuota;
        });
    }

    async function saveProject() {
        const payload = {
            name: state.project.name,
            inputs: state.project.inputs.map((input, index) => ({
                id: input.id,
                name: input.name,
                text: input.text,
                order: index,
            })),
        };

        try {
            const saved = await apiPut(`/api/projects/${state.project.id}`, payload);
            state.project = normalizeProject(saved);
            state.conflicts = [];
            state.solvingAt = null;
            lastSavedUtc = state.project.updatedUtc;
            updateAfterSave();
        } catch (error) {
            handleError(error);
        }
    }

    function updateAfterSave() {
        markClean();
        document.documentElement.dataset.lastSaved = lastSavedUtc;
        elements.projectName.textContent = state.project.name;
        renderAll();
    }

    async function loadProjectList() {
        try {
            const projects = await apiGet('/api/projects');
            elements.projectsList.innerHTML = '';
            projects.forEach(project => {
                const anchor = document.createElement('button');
                anchor.type = 'button';
                anchor.className = 'list-group-item list-group-item-action';
                anchor.textContent = `${project.name} (updated ${formatTime(project.updatedUtc)})`;
                anchor.addEventListener('click', async () => {
                    try {
                        const data = await apiGet(`/api/projects/${project.id}`);
                        state.project = normalizeProject(data);
                        state.conflicts = [];
                        state.dirty = false;
                        state.solvingAt = null;
                        lastSavedUtc = state.project.updatedUtc;
                        openProjectModal.hide();
                        updateAfterSave();
                    } catch (error) {
                        handleError(error);
                    }
                });
                elements.projectsList.appendChild(anchor);
            });
        } catch (error) {
            handleError(error);
        }
    }

    function updateQuota(newQuota) {
        state.quota = newQuota;
        renderQuota();
    }

    async function apiGet(url) {
        const response = await fetch(url, {
            headers: {
                'Accept': 'application/json',
            },
        });
        return handleApiResponse(response);
    }

    async function apiPut(url, body) {
        const response = await fetch(url, {
            method: 'PUT',
            headers: buildHeaders(),
            body: JSON.stringify(body),
        });
        const data = await handleApiResponse(response);
        await refreshQuota();
        return data;
    }

    async function apiPost(url, body, tokenCost) {
        const headers = buildHeaders();
        if (tokenCost !== TokenService.NONE) {
            headers['X-Client-LastSavedUtc'] = lastSavedUtc;
        }

        const response = await fetch(url, {
            method: 'POST',
            headers,
            body: body ? JSON.stringify(body) : null,
        });
        const data = await handleApiResponse(response);
        if (tokenCost !== TokenService.NONE) {
            await refreshQuota();
        }
        return data;
    }

    async function refreshQuota() {
        try {
            const quota = await apiGet('/api/users/quota');
            updateQuota(quota);
        } catch (err) {
            console.error('Failed to refresh quota', err);
        }
    }

    async function handleApiResponse(response) {
        if (response.ok) {
            if (response.status === 204) {
                return null;
            }
            return await response.json();
        }

        let payload;
        try {
            payload = await response.json();
        } catch {
            payload = { error: 'unknown_error' };
        }

        const error = new Error(payload.error || 'request_failed');
        error.status = response.status;
        throw error;
    }

    function handleError(error) {
        if (error.status === 409) {
            showToast('Please save the project before running this action.');
        } else if (error.status === 429) {
            showToast('Daily budget exceeded or rate limited.');
            state.overQuota = true;
            updateButtonStates();
        } else {
            showToast(error.message || 'Unexpected error occurred.');
        }
        console.error(error);
    }

    function showToast(message) {
        elements.toastMessage.textContent = message;
        bootstrapToast.show();
    }

    function canRunActions() {
        return !state.dirty && !state.overQuota;
    }

    function renderQuotaText(quota) {
        const remaining = Math.max(0, quota.dailyBudget - quota.dailySpent);
        return `Quota: ${quota.dailySpent}/${quota.dailyBudget} tokens (remaining ${remaining})`;
    }

    function buildHeaders() {
        const headers = {
            'Content-Type': 'application/json',
            'Accept': 'application/json',
            'X-CSRF-Token': csrfToken,
        };
        return headers;
    }

    function formatTime(value) {
        const date = new Date(value);
        return date.toLocaleTimeString();
    }

    function escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text ?? '';
        return div.innerHTML;
    }

})();
