% Notation of the help pages (Jorge, 2026-11-24): VO2 with a subscript 2 in
% prose; code keeps VO2. LaTeX (PDF manual) cannot print the Unicode
% subscript, so it gets \textsubscript; other outputs get the character.
\newcommand{\VO}{\ifelse{latex}{\out{VO\textsubscript{2}}}{\enc{VO₂}{VO2}}}
\newcommand{\VOmax}{\ifelse{latex}{\out{VO\textsubscript{2}max}}{\enc{VO₂max}{VO2max}}}
\newcommand{\VOpeak}{\ifelse{latex}{\out{VO\textsubscript{2}peak}}{\enc{VO₂peak}{VO2peak}}}
\newcommand{\VCO}{\ifelse{latex}{\out{VCO\textsubscript{2}}}{\enc{VCO₂}{VCO2}}}
