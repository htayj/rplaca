# Appearance Decisions: Version-1 Contract

- **Status:** normative implementation contract; no appearance system is complete
- **Date:** 2026-07-26
- **Applies to:** RPLACA on pinned McCLIM `1.0.0-koliada`
- **Companion documents:** [customization plan](APPEARANCE-CUSTOMIZATION-PLAN.md),
  [Genera-informed companion plan](GENERA-INFORMED-APPEARANCE-PLAN.md),
  [design posture](DESIGN.md), [stability boundary](STABILITY.md), and
  [known McCLIM/Drei defects](MCCLIM-ISSUES.md)

This document resolves every item in the companion plan's “Decisions to refine
before implementation” table. It is the version-1 contract to implement. It
does not edit, supersede as historical records, or claim that either planning
document has been implemented.

`54409068 feat(appearance): Define orthogonal style specifications` was made
before this resolution. Its declaration-only scope is directionally consistent
with Sections 2 and 3 below, but it is **not** evidence that the contract is
implemented. Before work resumes, audit that commit against this document;
later commits must amend it where its provisional API differs from the
contract.

## Evidence and portability labels

The labels below make the dependency boundary explicit.

| Label | Meaning |
|---|---|
| **Portable CLIM** | Required by CLIM 2 semantics; no McCLIM-private behavior is assumed. |
| **McCLIM extension** | Public API in the pinned McCLIM tree, not part of portable CLIM. |
| **Pinned implementation fact** | Verified against `1.0.0-koliada`; do not generalize to another CLIM implementation or McCLIM revision. |
| **Application policy** | Deliberate RPLACA behavior, independent of framework capability. |

The decisive evidence is:

- CLIM separates immutable text styles from inks/designs and permits dynamic
  drawing bindings; see the CLIM specification’s “Medium Components,” “Text
  Style Binding Forms,” and “Pane Properties” material surfaced through the
  local `mcclim-manual` skill.
- [`guix.scm`](../guix.scm) pins the McCLIM source revision to
  `1.0.0-koliada`. [`scripts/assert-mcclim-provenance.lisp`](../scripts/assert-mcclim-provenance.lisp)
  asserts only that the loaded source is below the Guix McCLIM source root; it
  does not establish the revision or any API contract. The pinned McCLIM
  source itself establishes that its public (but non-portable) font extension
  provides `clim-extensions:port-all-font-families`,
  `clim-extensions:font-family-all-faces`, and
  `clim-extensions:font-face-text-style`.
- In the current RPLACA launch path, `make-application-frame` has no
  preselected `:frame-manager`: it makes the frame before the standard frame
  manager adopts it, and adoption then generates panes. Basic pane foreground,
  background, and text-style defaults are installed when its medium is
  engrafted. Consequently a port-enumerated font is not available early enough
  for ordinary initial pane initargs on that path. McCLIM's explicit
  `:frame-manager` route may provide a staging path, but is deferred until the
  focused probe proves it preserves the application's frame and Drei safety
  invariants.
- The historical inspiration is documented, not copied, in the public
  [Genera inks, faces, and character styles note](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/inks-faces-and-character-styles.md),
  its [resident-font companion](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/extracting-resident-fonts.md),
  [gray-pattern discussion](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/genera/gray-patterns-and-stipples.md),
  [color-system discussion](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/color-systems-and-color-editor.md),
  and [CLIM II comparison](https://github.com/htayj/lisp-machine-container-museum/blob/main/docs/clim-2-on-genera.md).

## 1. Resolved decision register

| Decision | Settled version-1 contract | Evidence / label |
|---|---|---|
| Relationship to plans | This document is normative for version 1; companion-plan amendments win on conflict. Preserve both plans as rationale. | Application policy |
| Appearance model | Orthogonal typography, foreground ink, surface/background, and decoration axes. | Portable CLIM architecture |
| Theme model | Immutable coordinated role-overlay maps, never a global current-theme cache. | Application policy |
| Role composition | Resolve and overlay `surface < content < state`. | Application policy using portable bindings |
| `:selector-selected` | Adapt the legacy wire face to `(:selector-entry :selector-selection)`. | Current source / application policy |
| Default theme | `:classic`; it must be visually inert. | Regression policy |
| First extra theme | `:dark`, with the exact palette in Section 5. | Product decision, accessibility proof |
| Scope | Profile, bundle, revisions, candidate, and font generation are frame-local. | Portable CLIM frame ownership |
| Font resolution | Resolve enumerated fonts per target port through public enumeration. | McCLIM extension |
| Font invalidation | Explicit Refresh Font Inventory, then increment only the owning frame generation. | McCLIM extension |
| Relative sizes | Only CLIM logical ladder and one-step `:smaller`/`:larger`. | Portable CLIM vocabulary |
| Numeric relative sizes | Reject them. | Portability safeguard |
| Literal device font | Deferred advanced feature with declared portable fallback. | McCLIM-specific future work |
| Text-style mappings | Never mutate them in ordinary activation. | Pinned implementation fact |
| Compose/minibuffer font | Fixed-width only; construction/restart-only. | Drei/geometry safety policy |
| Live activation | All axes in a candidate activate together or none do. | Application transaction policy |
| Unsafe candidate | Preview and optionally save for next startup; do not call it active. | Application policy |
| Selection | Existing marker plus state typography/ink; never color alone. | Current behavior / portable CLIM |
| Filled selection | Deferred. | Portable CLIM limitation |
| Persisted inks | Opaque RGB and standard ink tokens only. | Portable-safe persistence subset |
| Patterns/stipples/opacity | Excluded from version 1. | Portability and scope policy |
| Built-in contrast | At least 4.5:1 for every effective text stack. | Accessibility policy |
| User/package contrast | Warning by default, optional strict validation, never automatic recoloring. | Application policy |
| Package theme removal | Transactional fallback or refuse/defer unload. | Application transaction policy |
| Styled content | Explicit non-goal. | Domain/display separation |
| Safe Reload | Reconcile declarations only; do not reread or save user appearance data. | Application policy |
| Pointer documentation | Leave unchanged. | Pinned implementation fact |
| Renderer internals | Prohibited. | CLIM output-record/redisplay discipline |

## 2. Declaration model and semantic roles

### 2.1 Immutable objects

Declarations and resolved data are immutable after publication:

```text
appearance-typography-spec   family, face, size
appearance-ink-spec          foreground
appearance-surface-spec      background
appearance-decoration-spec   kind, parameters
appearance-role-style        the four partial axes
appearance-role-definition   id, kind, documentation, fallback, axes, owner
appearance-theme-definition  id, documentation, parent, role overlays, owner
appearance-profile           selected theme, strict-contrast boolean,
                             immutable role overrides
resolved-role-style          text style, foreground, background, decoration,
                             provenance, render key
resolved-appearance-bundle   catalog/profile/font generations, port identity,
                             roles, surface defaults, bundle key
```

Version 1 admits only the `:selection-marker` decoration with the precise
parameter form `(:marker STRING)`; `parameters` is not a generic drawing-data
extension point.

The internal unspecified sentinel represents inheritance. In every role style
component, user-visible `nil` is invalid; `:none` is valid only where an axis
has a real absence meaning, initially decoration. The public keyword
`:unspecified` is also invalid as a leaf value and cannot stand for the private
inheritance sentinel. `nil` is valid only as the
boolean value of profile/configuration `:strict-contrast`. Typography
components inherit independently. A compound bold-italic face must be
specified explicitly; resolution must not invent it from separately encountered
bold and italic declarations.

Immutability is **deep** at every application API boundary. Construction copies
and freezes every caller-supplied string, list, vector, hash-table value,
decoration parameter, role-overlay entry, override entry, provenance path, and
catalog association before publication. Accessors either expose immutable
value objects or return fresh deep copies; no caller-owned mutable sequence is
retained and no published mutable sequence is returned. A bundle builder may
allocate private temporary structures, but it never mutates a profile,
declaration, or resolved bundle that it did not create for that build.

This application invariant is stronger than the CLIM guarantee that its text
styles and designs are immutable objects: it also covers the Lisp containers
which describe them.

**Classification:** portable CLIM supports immutable style objects; the
declaration and bundle structures are application policy.

### 2.2 Role kinds and vocabulary

Fallbacks must have the same kind in version 1. Cross-kind fallbacks are an
error.

| Kind | Roles |
|---|---|
| Surface | `:base`, `:transcript-pane`, `:info-pane`, `:compose-pane`, `:minibuffer-pane`, `:help-pane`, `:pointer-documentation` |
| Content | `:default-text`, `:transcript-user`, `:transcript-agent`, `:transcript-tool`, `:transcript-system`, `:transcript-empty`, `:system`, `:error`, `:tool-result`, `:modeline`, `:selector-title`, `:selector-header`, `:selector-entry`, `:selector-separator`, `:selector-footer` |
| State | `:selector-selection`, `:minibuffer-selection-emphasis`, `:disabled` |

Surface roles may specify all axes. Content and state roles may specify
typography, foreground, and decoration; background/surface is rejected for
them. Unknown catalog/configuration roles are errors. Unknown display adapter
roles render as `:default-text` and record one bounded diagnostic per catalog
generation.

### 2.3 Exact legacy wire adapter

| Existing entry `:face` | Role stack |
|---|---|
| `:default-text` | `(:default-text)` |
| `:selector-title` | `(:selector-title)` |
| `:selector-header` | `(:selector-header)` |
| `:selector-entry` | `(:selector-entry)` |
| `:selector-selected` | `(:selector-entry :selector-selection)` |
| `:selector-separator` | `(:selector-separator)` |
| `:selector-footer` | `(:selector-footer)` |
| `:tool-result` | `(:tool-result)` |
| `:system` | `(:system)` |
| `:disabled` | `(:default-text :disabled)` |
| `:error` | `(:error)` |

The legacy `:selector-selected` wire stack remains
`(:selector-entry :selector-selection)`. In `:classic`, its shared selection
state retains the existing marker and foreground but is deliberately not bold.
The minibuffer selected candidate uses
`(:minibuffer-pane :selector-entry :selector-selection
:minibuffer-selection-emphasis)` so its real first-party control boundary
retains the existing `">"` marker and bold emphasis. Existing presentations
stay in place; roles never replace presentation types, translators, or input
contexts.

## 3. Resolution, typography, and render keys

### 3.1 Exact cascade

Resolve source layers from low to high:

```text
built-in defaults
< root-to-leaf parent themes
< selected theme
< appearance.sexp overrides
< init.lisp startup overrides
< environment overrides
< command-line overrides
< unsaved frame-local overrides
```

Within each layer, walk a role’s fallback chain root-to-requested role and
merge each partial component. Then merge layers from low to high. A base-role
override consequently affects its fallbacks, while a same-layer specific role
override wins.

For a display stack, resolve roles independently then overlay surface, content,
and state roles in stack order. A render key is structural: it contains only
the effective output of the role stack, not a raw profile revision. No-op candidates therefore do
not invalidate incremental output; a transcript-user change does not invalidate
agent output.

**Classification:** cascade and cache-key policy are application policy;
`with-drawing-options` and `with-text-style` at the final output boundary are
portable CLIM.

### 3.2 Typography contract

Portable logical sizes are exactly:

```text
:tiny :very-small :small :normal :large :very-large :huge
```

Only `:smaller` and `:larger` are relative. Consume each relative component
once at its cascade point, then clamp within that ladder. A relative component
over a number, unresolved size, or device font signals
`relative-size-base-invalid`. Numeric relative scaling is deliberately absent.

The dynamic output boundary uses `clim:with-text-style` or the narrower CLIM
text-family/face/size bindings, with the final composed style. It never alters
the medium default, port mappings, pane geometry, profile, or catalog.

## 4. Fonts and port timing

### 4.1 Descriptor and resolution boundary

Version 1 supports:

1. Portable text-style descriptors using CLIM family, face, and logical or
   numeric size values.
2. Enumerated descriptors containing user-facing family name, face name, and
   size data. They are resolved only against the target port.

For enumerated descriptors, call
`clim-extensions:port-all-font-families`, exactly match one family display
name, enumerate faces with `clim-extensions:font-family-all-faces`, exactly
match one face display name, validate scalable/fixed-size status, obtain the
style with `clim-extensions:font-face-text-style`, and validate mapping/metrics
on that port. Duplicate names are ambiguity errors. Display names are never
reconstructed into backend text-style strings.

`clim-extensions:port-all-font-families :invalidate-cache t` is the sole
inventory-refresh mechanism. A refresh, port change, frame recreation on
another port, or catalog reload invalidates only the relevant bundle generation.
Negative lookup caching is limited to that generation.

**Classification:** public McCLIM extension. The result must not be described
as portable CLIM behavior.

### 4.2 Corrected pane timing

The initial plan’s port-before-pane path is not available through the current
ordinary `make-application-frame` launch path: the frame is instantiated before
the standard frame manager adopts it, and panes are generated during adoption.
An explicitly selected `:frame-manager` might expose a port earlier, but no
such path is introduced until a focused proof establishes that it preserves
normal frame ownership and Drei behavior. Therefore version 1 will:

- pass a portable profile into frame construction;
- use portable/default logical pane values at construction;
- resolve named port fonts after adoption; and
- classify named pane-default typography as restart-required until a public
  staging path is separately proven.

Never mutate `(setf clim:text-style-mapping)` during profile activation. In the
pinned implementation this is a port-level mapping cache shared beyond one
frame.

Named fonts are initially allowed only for transcript, help, info/modeline,
and noneditable structured output. Compose and minibuffer must be fixed-width;
compose live typography is forbidden pending a Drei stress proof.

## 5. Built-in profiles and accessibility

### 5.1 `:classic`

`:classic` is a behavioral golden profile, not a repaint of backend defaults.
It must not pass `:foreground` or `:background` overrides to pane construction.
All RPLACA application panes independently use the portable CLIM logical
`(:fix :roman :normal)` text style as their construction default, including
the Drei compose pane. Existing current output color values remain exact:

| Current output | Existing value |
|---|---|
| transcript user | `(0.10 0.25 0.55)` |
| transcript agent | `(0.10 0.10 0.10)` |
| transcript tool / tool result | `(0.12 0.34 0.18)` |
| transcript system | `(0.36 0.36 0.36)` |
| selector title/header/footer/selected/entry | `(0.16 0.22 0.45)`, `(0.18 0.36 0.20)`, `(0.35 0.35 0.35)`, `(0.10 0.38 0.65)`, `(0.20 0.20 0.20)` |
| generic system/disabled/error | `(0.45 0.45 0.45)`, `(0.45 0.45 0.45)`, `(0.60 0.12 0.12)` |
| selected minibuffer candidate | `">"` marker plus `:bold` |

This matches the current literals in `src/mcclim-interface.lisp`; it does not
claim a backend-independent RGB background.

### 5.2 Exact `:dark` palette

All listed values are opaque RGB. Pointer documentation deliberately has no
dark override in version 1.

| Kind | Role | Value |
|---|---|---|
| Surface | `:base`, `:transcript-pane`, `:compose-pane`, `:help-pane` | `#0D1117` |
| Surface | `:info-pane`, `:minibuffer-pane` | `#161B22` |
| Surface | `:pointer-documentation` | unchanged / not activated |
| Content | `:default-text`, `:transcript-agent`, `:modeline`, `:selector-entry` | `#E6EDF3` |
| Content | `:transcript-user` | `#79C0FF` |
| Content | `:transcript-tool`, `:tool-result`, `:selector-header` | `#7EE787` |
| Content | `:transcript-system`, `:system` | `#B1BAC4` |
| Content | `:transcript-empty`, `:selector-separator`, `:selector-footer` | `#8B949E` |
| Content | `:error` | `#FF7B72` |
| Content | `:selector-title` | `#A5D6FF`, bold |
| State | `:selector-selection` | `#FFFFFF`, existing marker |
| State | `:disabled` | `#8B949E` |

Computed WCAG relative-luminance ratios for every active stack are:

| Effective stack | Ratio |
|---|---:|
| default/agent/help/compose on `#0D1117` | 16.02:1 |
| user on `#0D1117` | 9.73:1 |
| tool on `#0D1117` | 12.32:1 |
| system on `#0D1117` | 9.63:1 |
| empty on `#0D1117` | 6.15:1 |
| error on `#0D1117` | 7.51:1 |
| modeline on `#161B22` | 14.64:1 |
| selector title/header on `#161B22` | 11.25:1 / 11.26:1 |
| selector entry on `#161B22` | 14.64:1 |
| separator/footer/disabled on `#161B22` | 5.62:1 |
| selected selector entry on `#161B22` | 17.30:1 |

Dark changes palette axes only. Its typography is identical to `:classic`, so
the existing title boldness and minibuffer-only selected-candidate emphasis
remain in force, while the ordinary selector selection retains its marker
without acquiring a new bold declaration.

Built-ins fail validation below 4.5:1 after their final surface/content/state
composition. The large-text exception is not used. A stack whose declarations
and overlays are entirely untouched built-ins is always fatal at activation and
Save; `:strict-contrast` cannot relax it. "Untouched built-in" means every
effective axis contribution has the explicit declaration owner `:builtin` and
no appearance-file, `init.lisp`, environment, command-line, unsaved, or profile
override contributes to the resolved stack. A missing (`nil`) owner is never
treated as built-in. Any stack touched by a user,
appearance-file, `init.lisp`, environment, command-line, interactive, or
package declaration/override follows the user/package rule: issue a typed
warning containing the resolved stack and ratio when strict validation is
disabled; with `:strict-contrast t`, make that same condition fatal and block
Apply and Save. The default is `:strict-contrast nil`. Colors are never
silently adjusted.

### 5.3 Activation UX

At startup, parse and validate the profile before frame creation. `:classic`
constructs exactly as today. `:dark` may supply known opaque surface/default
colors during ordinary pane construction, but named font choices resolve after
adoption.

Interactive theme selection creates a staged immutable candidate and renders
it in a separate CLIM preview pane. It does not alter the main frame.

- **Apply** succeeds only when every changed axis is
  `:render-boundary-live`; publish the full profile/bundle atomically, then
  request ordinary redisplay.
- Any candidate changing a pane surface, pane default foreground, or pane
  default text style is `:restart-required` in version 1. Apply reports that
  result and leaves the active profile unchanged; it may not apply only the
  content colors.
- **Save** explicitly persists a valid staged candidate. For
  restart-required candidates it says “Saved for next start; active appearance
  unchanged.” Save does not imply Apply. An untouched built-in contrast failure
  always makes the candidate invalid. A contrast warning in a user/package
overlay is saveable when `:strict-contrast nil`; with `:strict-contrast t`,
it is invalid and neither Apply nor Save is offered.

The `appearance-contrast-warning` payload is exact: `origin` is the copied
resolved-axis provenance, `role` and `path` are the copied effective role
stack, `axis` is `:contrast`, `value` is the computed ratio (or `nil` for an
ink outside the opaque RGB/standard-token subset), `port` and
`available-choices` are `nil`, `fatal-p` records the policy result, and
`suggested-repairs` is `(:increase-foreground-contrast :change-surface)`.
The condition is signaled as a warning when nonfatal and as the same typed
condition through the error boundary when fatal. Neither path changes a color.

Declaration contribution origins are structured exactly as
`(:role-default ROLE-ID :owner OWNER)` and
`(:theme THEME-ID :owner OWNER)`. Package role defaults remain in the existing
lowest-precedence defaults layer, and package theme overlays remain in the
existing parent/selected-theme layers; package registration adds no source
cascade layer. At stack composition, provenance retains only the final
contributor for each effective axis.

An untouched `:classic` stack remains contrast-inert because its backend
surface is deliberately unspecified. Once a user or package contribution
touches that stack, it follows the same warning/strict policy and is validated
even though the selected theme remains `:classic`.
- **Revert** discards the candidate. **Describe** distinguishes active, staged,
  and persisted profiles.

Automatic frame recreation is deferred. It needs a separate proof of draft
preservation, multi-frame isolation, ESA/Drei input ownership, rollback, and
clean teardown. Users activate a saved surface-changing profile at next start.

**Classification:** `with-drawing-options` and output redisplay are portable
CLIM; pane-default asymmetry and this conservative activation classification
are pinned-McCLIM facts plus application policy.

## 6. Configuration, command line, and environment

### 6.1 Data-only file grammar

`appearance.sexp` contains exactly one form:

```lisp
(:rplaca-appearance
 :version 1
 :theme (:package "org.example.plugin" "outline-dark")
 :strict-contrast nil
 :overrides
 ((:transcript-user :foreground (:rgb 0.478 0.635 0.969))
  ((:package "org.example.plugin" "outline-heading")
   :foreground (:rgb 0.478 0.635 0.969))
  (:base :typography (:portable :family :fix :face :roman :size :normal))
  (:selector-selection :typography (:face :bold)
                       :decoration :selection-marker)))
```

The parser binds `*read-eval*` to `nil`, accepts one bounded/depth-limited form,
rejects trailing forms and unknown clauses, and atomically replaces only a
fully valid file. `:strict-contrast` is a boolean and defaults to `nil` when
omitted. It is copied into the immutable profile and governs the selected
user/package candidate after all source layers resolve; an untouched built-in
stack remains fatal regardless of its value. It structurally preserves a
package ID as the exact tagged list `(:package "owner" "local")` in `:theme`
and in every override role position; it does not resolve the ID against the
later catalog while parsing. It accepts no CLIM objects, arbitrary Lisp,
opacity, patterns, stipples, ALUs, indexed maps, or executable palette forms.
Omission inherits; role component `nil` is rejected; `:none` is accepted only
for decoration.

Portable typography is `(:portable :family FAMILY :face FACE :size SIZE)`.
Enumerated typography is `(:enumerated :family "display name" :face "display
name" :size SIZE)`. The latter remains data until port resolution.

### 6.2 External selector grammar and precedence

Environment and command-line input is deliberately narrower than the file:

```text
RPLACA_APPEARANCE_THEME=THEME-ID
RPLACA_APPEARANCE_OVERRIDES=OVERRIDE-LIST
--appearance-theme THEME-ID | --appearance-theme=THEME-ID
--appearance-override ROLE-ID=OVERRIDE-FORM | --appearance-override=ROLE-ID=OVERRIDE-FORM
--appearance-file PATH | --appearance-file=PATH
```

At the external boundary, `THEME-ID` and `ROLE-ID` are text: a core ID is its
keyword name without `:` (`classic`, `dark`, `transcript-user`), and a package
ID is exactly `owner/local` (for example,
`org.example.plugin/outline-dark`). The boundary parser converts only the
latter to the persisted/catalog datum `(:package "owner" "local")`; no
command-line or environment string is a Lisp package symbol or an accepted
tagged list.

`OVERRIDE-FORM` is exactly one bounded data-only axis plist:

```lisp
(:foreground (:rgb R G B)
 :typography (:portable :family FAMILY :face FACE :size SIZE)
 :decoration :selection-marker)
```

Each of the four axis keys (`:foreground`, `:background`, `:typography`,
`:decoration`) may appear at most once; at least one axis is required. The
form has at most four axes, is at most 4,096 UTF-8 bytes, and has reader depth
at most 8. An RGB ink is exactly `(:rgb R G B)`, with three finite real
numbers in `[0,1]`. The only standard ink tokens are `:black`, `:white`,
`:red`, `:green`, `:blue`, `:cyan`, `:magenta`, `:yellow`, and `:gray`.
Opacity, patterns, stipples, arbitrary designs, and every other token are
rejected. Role-kind validation still rejects an otherwise well-formed axis
which that role may not carry.

`OVERRIDE-LIST` is one bounded data-only list of at most 64 pairs
`("ROLE-ID" OVERRIDE-FORM)`, serialized in at most 32,768 UTF-8 bytes. For
example:

```text
--appearance-override=org.example.plugin/outline-heading=(:foreground (:rgb 0.478 0.635 0.969))
RPLACA_APPEARANCE_OVERRIDES='(("org.example.plugin/outline-heading" (:foreground (:rgb 0.478 0.635 0.969))))'
```

The environment is singular: an operating-system environment variable cannot
occur repeatedly. Selector tokens are not whitespace-trimmed: whitespace is
not part of the identifier grammar and therefore makes the source invalid.

The command-line `--appearance-override` is repeatable. Parse its argument by
splitting at the first ASCII `=`: both the nonempty `ROLE-ID` and the nonempty
right-hand `OVERRIDE-FORM` are required, while any later `=` remains part of
the form. Duplicate role IDs in repeated command-line overrides, duplicate
role IDs in `OVERRIDE-LIST`, or duplicate axis keys in one `OVERRIDE-FORM`
invalidate that entire selector source; there is no last-one-wins merge.
`--appearance-theme` and `--appearance-file` occur at most once. A missing
option argument, duplicate singleton option, empty value, malformed
identifier, missing first `=`, or invalid bounded data form likewise
invalidates that *selector source*.

There are two intentionally different recovery rules:

- A missing default `appearance.sexp` is normal and contributes no file layer.
  An explicitly selected file, or the default file when it exists, which is
  unreadable, malformed, has trailing data, or fails validation is an invalid
  file. Startup then uses `:classic`, records one file diagnostic, and does
  not merge init, environment, or command-line appearance selections. A live
  file reload instead retains the last valid frame profile.
- An invalid environment source is ignored as a whole, records one source
  diagnostic, and retains the lower valid candidate. An invalid command-line
  source is treated identically; therefore command line overrides a valid
  environment source only when the entire command-line source is valid.
  A present-but-empty environment variable is invalid, while an absent one has
  no effect.

`--appearance-file` selects the file layer rather than moving it in the
precedence order. The exact precedence is Section 3.1. Environment and
command-line selectors are applied after `init.lisp` and package declarations
so referenced themes can exist. `--no-init` skips executable initialization
only; it does not skip the appearance file. A future appearance-specific
opt-out must be explicit.

An option-syntax failure for `--appearance-file` (missing, duplicate, or empty
argument) is an invalid command-line selector source and retains the lower
valid candidate. A successfully parsed file path which cannot be read or
validated is instead an invalid file and follows the `:classic` fallback rule
above.

Only the GUI branch performs appearance startup resolution: after the existing
initialization and package-registration phases, and immediately before its one
`clim:make-application-frame` call, it builds and validates an immutable
startup profile and passes it as `:appearance-profile`. Prompt-only/one-shot
execution returns before the appearance file reader, selector resolver, font
enumeration, profile construction, or any appearance write; it must not cause
GUI appearance side effects.

### 6.3 Package-owned identifier grammar

Core IDs are keywords. Package IDs are persisted tagged data, never package
symbols:

```lisp
(:package "org.example.plugin" "outline-heading")
```

Persisted and catalog references use this tagged datum exactly: file `:theme`,
file override role IDs, `appearance-theme-definition` parent IDs,
`appearance-role-definition` fallback IDs, and every role/theme catalog key.
The parser preserves the three-element tagged structure before later catalog
resolution. UI presentation objects likewise carry the tagged datum as their
semantic value, but display it as `owner/local`; completion returns the stored
presentation value rather than reparsing display text. Command line and
environment use the external `owner/local` form defined in Section 6.2 and
convert it at that boundary only.

The first tagged string is the textual owner and the second is the textual
local identifier. Normalize both with ASCII lowercase before validation; a
stored or supplied spelling that changes under normalization is rejected rather
than silently rewritten. `owner` is a dot-separated sequence of
`[a-z][a-z0-9-]*`; `local` is `[a-z][a-z0-9-]*`; each is at most 128
characters. Reject non-ASCII, empty, malformed, duplicate elements, and
unknown tags. The normalized pair `(owner, local)` is the unique catalog key
in its role or theme namespace.

An extension may register a declaration only when its textual owner equals the
canonical `package-definition-name` of the currently active package definition
and that definition's resource allowlist contains `:appearance`. Core IDs are
reserved to RPLACA: packages cannot define, replace, or alter them. An
extension's parent-theme and role-fallback references may name only a core ID
or an ID owned by that same package; all cross-owner references are rejected.
Candidate collisions with an existing other owner's declaration are fatal, and
duplicate IDs inside a staging set are fatal.

Registration is an owner-scoped staged reload. Build a candidate catalog that
replaces only the submitting owner's prior declarations, validate its complete
graphs and all affected frames, then atomically publish it. Thus a package can
replace its own mutually referring roles/themes in one staged set, but cannot
claim, remove, or collide with RPLACA or another package's declarations.
Persist the tagged lists as data—not interned symbols—so unloading a Common
Lisp package cannot make preferences unreadable.

## 7. Commands, editor, transactions, and reload

### 7.1 Command and key compatibility

The exact public operation symbols are:

| User-visible operation | UI-independent helper | Frame command |
|---|---|---|
| Switch Appearance Theme | `switch-appearance-theme-command` | `com-chat-switch-appearance-theme` |
| Describe Current Appearance | `describe-current-appearance-command` | `com-chat-describe-current-appearance` |
| Customize Appearance | `customize-appearance-command` | `com-chat-customize-appearance` |
| Apply Staged Appearance | `apply-staged-appearance-command` | `com-chat-apply-staged-appearance` |
| Save Appearance | `save-appearance-command` | `com-chat-save-appearance` |
| Revert Staged Appearance | `revert-staged-appearance-command` | `com-chat-revert-staged-appearance` |
| Reload Appearance File | `reload-appearance-file-command` | `com-chat-reload-appearance-file` |
| Refresh Font Inventory | `refresh-font-inventory-command` | `com-chat-refresh-font-inventory` |

Only `C-h F` and `C-c F` are added appearance bindings, and both dispatch
`com-chat-customize-appearance`. The existing `C-h f` and `C-c f` remain
Describe Function; `C-c t` remains Toggle Tool Results; and `C-x F` remains
the bitmap Font Editor. Apply, Save, Revert, Reload, and Refresh are reached
through the static menu, `M-x`, or presentations—no raw preview/editor
gestures are preserved or introduced.

`customize-appearance-command` is the UI-independent helper: it builds or
focuses the frame-local staged candidate and returns a command result without
performing display work. The frame command is exactly
`com-chat-customize-appearance`; it obtains the application frame, invokes
that helper, and requests normal CLIM redisplay. A static Appearance menu
is named `rplaca-chat-appearance-menu` and contains a `Customize Appearance`
item dispatching that frame command; it is not assembled by a display function
or a mutable process-global command table.

`customize-face-command` is a deprecated compatibility function which calls
`customize-appearance-command` and emits one bounded deprecation diagnostic
per process. It must not restore the removed global-face mechanism. Do not add
an interim `customize-appearance` alias: it is deliberately absent as a
function, command, menu spelling, and key target so callers use the precise
new names above. `customize-drawing-style-command` is likewise explicitly
rejected: it is not a compatibility alias, command, menu entry, or key target.

The editor is a command/presentation interface. Its candidates, roles, font
choices, activation capability, and diagnostics are presentations. The
`accepting-values` experiment is deferred rather than a version-1 gate: until
a focused ESA/Drei probe establishes cancellation and input-loop recovery, the
editor uses commands, presentations, and existing completion in a dedicated
application preview pane.

### 7.2 Transaction boundary

All candidate causes—interactive edits, file reload, package catalog change,
or theme selection—resolve and validate the *whole* candidate against its
frame/port before publication. On failure retain the previous profile, bundle,
and panes. A display function never resolves, persists, or mutates appearance.

Typed conditions include unknown role, invalid component, role/theme cycle,
missing parent, unsupported axis, font ambiguity/unavailability, invalid
relative base, contrast warning, live-update unsupported, and activation
failure. They carry origin, role, axis, value, path, port, bounded choices,
fatality, and suggested repair.

Condition classes are public types for handlers. Application code constructs
and signals them only through the appearance condition factory/signaling
boundary, which deep-copies diagnostic payloads; callers must not rely on
arbitrary `CL:MAKE-CONDITION` construction to provide that guarantee.

### 7.3 Safe Reload and package removal

Safe Reload does not reread `appearance.sexp`, rerun `init.lisp`, or save a
candidate. It builds a candidate declaration catalog, validates it, resolves
replacement bundles for all affected frames, classifies every transition, and
publishes only if all steps succeed. Failed reconciliation leaves the old
catalog and bundles published.

Packages may register owner-qualified roles, themes, and portable default
overlays only. They may not mutate frame profiles/bundles, install mappings,
register executable ink resolvers, reopen built-ins, attach appearance to
durable content, or introduce raster drawing state.

Unloading a package follows the same transaction. If removal makes the active
theme unavailable and `:classic` cannot be safely activated, refuse or defer
the unload; do not publish dangling declarations or partially fall back.

## 8. Explicit non-goals

Version 1 excludes:

- automatic frame recreation;
- live Drei or compose typography, cursor theming, and line-height/baseline
  mutation;
- filled selection backgrounds, underline, strikeout, boxes, borders,
  stipples, patterns, opacity, ALUs, indexed color maps, and arbitrary designs;
- literal device fonts, port text-style mapping edits, and application font
  fallback engines;
- pointer-documentation, menu-bar, scrollbar, and generic gadget-chrome
  theming;
- per-character, word, region, message, or buffer persistent styling;
- custom render loops, direct sheet/medium manipulation, coordinate hit
  testing, raw output-record mutation, or a replacement repaint engine; and
- Genera style indices, raster font names as portable families, native
  screen/printer maps, or executable serialized palette forms.

## 9. Required proof before and during implementation

### 9.1 Focused pinned-McCLIM probes

Run these through the Guix wrapper on the pinned tree, with Xvfb where GUI
behavior is involved:

1. Port/frame-manager availability before pane construction and a named-font
   construction experiment. The already established ordinary-frame result is
   “not early enough”; retain that restriction unless an explicit staging path
   proves otherwise.
2. Pane construction defaults and a live color experiment, including
   reinitialize, engrafted media, expose, resize, and rollback. No private
   adapter may be added if it fails.
3. Font enumeration round trip, duplicate-name ambiguity, scalable/fixed-size
   handling, text-style mapping, metrics, and explicit cache invalidation.
4. Relative text-style composition against the logical ladder and rejection of
   numeric/device bases.

Failure narrows scope; it never authorizes renderer internals.

The following are **exploratory post-version-1 probes**, not release gates and
not permission to expand the first implementation: `accepting-values` nesting
inside ESA/Drei; public filled-selection decoration preserving presentations;
and pointer-documentation styling without changing its generated layout. The
initial editor uses commands, presentations, and completion. Pointer
documentation remains untouched, and selection keeps its marker plus bold
state typography.

### 9.2 Deterministic and GUI gates

FiveAM must cover immutability, unspecified versus `:none`, axes, cycles,
layer/fallback/stack ordering, typography merge, relative sizes, classic
goldens, wire mapping, runtime/config unknown-role distinction, render-key
stability/locality, fake-port generation separation, font ambiguities,
conditions, transactions, parser bounds/round trips, precedence, package
rollback, and contrast.

The final GUI gate uses private Xvfb and covers multi-frame isolation, startup
classic/dark behavior, staged/save-for-restart behavior, resize/expose,
menus/keys/completion, Safe Reload/package failure rollback, existing
switch-buffer and compose-geometry proofs, pointer tracking, empty stderr, no
runtime-failure signature, and clean process teardown. Prefer semantic
snapshots; use only tolerant samples of known solid surfaces when pixels are
necessary.

## 10. Revised implementation sequence

Each commit is atomic, includes focused tests, and passes the appropriate Guix
container gate.

1. `feat(appearance): Define orthogonal style specifications` — audit and
   complete the declaration-only foundation against this contract.
2. `feat(appearance): Resolve semantic role cascades` — layers, fallbacks,
   stacks, provenance, relative sizes, and classic goldens.
3. `feat(ui): Route classic output through frame-owned appearance roles` —
   add profile state and role-local structural render keys while preserving
   presentations and proving the default is visually inert; it neither resolves
   ports nor creates a port bundle.
4. `feat(ui): Preserve classic pane construction under appearance state` —
   pass the immutable startup profile at frame construction, but introduce no
   named-port font or nonclassic pane-default promise.
5. `feat(appearance): Add accessible dark profile` — exact Section 5 palette,
   strict-contrast policy, and contrast tests.
6. `feat(ui): Activate color profiles transactionally` — render-boundary live
   activation only; restart-required candidates remain atomic.
7. `feat(config): Persist appearance profiles safely` — parser that preserves
   tagged package IDs structurally before catalog resolution, exact selector
   failure rules, precedence, atomic save, and prompt isolation.
8. `feat(appearance): Build frame-local port bundles` — port identity, font
   generations, structural keys, and fake ports.
9. `feat(appearance): Enumerate port font choices` — public enumeration,
   refresh, ambiguity/fixed-width validation.
10. `feat(ui): Add the CLIM appearance editor` — staged preview, commands,
    diagnostics, and compatibility aliases; use commands, presentations, and
    completion rather than `accepting-values`.
11. `feat(packages): Register owned appearance declarations` — tagged IDs,
    catalog transactions, Safe Reload, unload refusal/rollback.
12. `test(gui): Prove appearance lifecycle behavior` — complete pinned GUI
    lifecycle and stability proof.

No commit in this sequence may claim implementation of a deferred non-goal.
