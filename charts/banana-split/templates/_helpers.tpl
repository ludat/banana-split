{{/*
Expand the name of the chart.
*/}}
{{- define "banana-split.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "banana-split.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "banana-split.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "banana-split.labels" -}}
helm.sh/chart: {{ include "banana-split.chart" . }}
{{ include "banana-split.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "banana-split.selectorLabels" -}}
app.kubernetes.io/name: {{ include "banana-split.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "banana-split.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "banana-split.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{/*
Env for app
*/}}
{{- define "banana-split.env" -}}
- name: BANANASPLIT_DATABASE_URL
  valueFrom:
    secretKeyRef:
      name: {{ include "banana-split.fullname" . }}-db-app
      key: uri
{{- end }}

{{/*
Image tag used in the app
*/}}
{{- define "banana-split.tag" -}}
{{ .Values.image.tag | default (printf "%s%s" "v" .Chart.AppVersion) }}
{{- end }}
