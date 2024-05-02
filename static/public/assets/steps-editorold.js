import { LitElement, html, ref, createRef } from "./js/thirdparty/lit.js";

export class StepsEditor extends LitElement {
  static properties = {
    steps: [{}]
  }
  render() {
    return steps.map(step => html`
<div class="rounded-lg overflow-hidden border border-slate-200 group/item collectionStep">
	<input type="checkbox" id="stepState-0" class="hidden stepState" 
		>
	<div class="flex flex-row items-center bg-gray-50 ">
		<div class="h-full shrink bg-gray-50 p-3 hidden border-r border-r-slate-200">
			<svg class=" h-4 w-4">
				<use href="/assets/svgs/fa-sprites/solid.svg#grip-dots-vertical"></use>
			</svg>
		</div>
		<div class="flex-1 flex flex-row items-center gap-1 bg-white pr-5 py-3">
			<label for="stepState-0" class="p-3 cursor-pointer text-xs text-slate-700">1</label>
			<label for="stepState-0" class="p-3 cursor-pointer">
				<svg class="inline-block icon h-4 w-3 group-has-[.stepState:checked]/item:rotate-90">
					<use href="/assets/svgs/fa-sprites/solid.svg#chevron-right"></use>
				</svg>
			</label>
			<div class="w-full space-y-1 relative">
				<div class="absolute right-0 flex items-center gap-3 text-xs text-gray-600 hidden group-hover/item:flex">
					<button class="">View results</button>
					<button class="text-blue-600">
						<svg class="w-2 h-3">
							<use href="/assets/svgs/fa-sprites/solid.svg#play"></use>
						</svg>
					</button>
					<a class="text-red-700" _="on click remove the closest parent <.collectionStep/>">
						<svg class="w-2 h-3">
							<use href="/assets/svgs/fa-sprites/solid.svg#xmark"></use>
						</svg>
					</a>
				</div>
				<input class="text-lg w-full" placeholder="Untitled" value="" name="[0][title]" id="title-0">
				<div class="relative flex flex-row gap-2 items-center">
					<label for="actions-list-input-0" class="w-28  shrink text-sm font-medium form-control ">
					<input list="actions-list" id="actions-list-input-0" class="input input-sm input-bordered w-full" placeholder="method" value="GET" _="on change throttled at 500ms put \`[0][\${me.value}]\` into #actions-data-0's @name "></label>
					<label for="actions-data" class="flex-1 text-sm font-medium form-control w-full ">
						<div class="flex flex-row items-center gap-1"><input type="text" id="actions-data-0" class="input input-sm input-bordered w-full" placeholder="Request URI" name="[0][GET]"></div>
					</label>
				</div>
			</div>
		</div>
	</div>
	<div class="border-t border-t-slate-200 space-y-3 p-3 hidden group-has-[.stepState:checked]/item:block">
		<div role="tablist" class="tabs tabs-bordered pt-1">
			<input type="radio" name="_httpOptions-0" role="tab" class="tab" aria-label="Params" checked="">
			<div role="tabpanel" class="tab-content px-2 py-4 space-y-2" id="[0][params]">
				<div class="flex flex-row items-center gap-2 paramRow">
					<span class="shrink hidden assertIndicator">✅</span><input class="shrink input input-xs input-bordered w-1/3" placeholder="Key" value="" data-keyprefix="[0][params]" _="on change set :kPrefix to \`\${my @data-keyPrefix}[\${me.value}]\` then set (next <input/>)'s @name to :kPrefix" >
					<div class="shrink flex flex-row gap-1 items-center">
						<a _="on click set :nextIndex to (the closest parent <div.paramRows/>).childNodes.length 
							then set :paramTmpl to 
							#paramRowTmpl.innerHTML.replaceAll(#stepsForm's @data-defaultKeyPrefix, @data-keyPrefix).replaceAll('[aidx]', \`[\${:nextIndex}]\`)
							then put :paramTmpl after the closest parent <div.paramRow/>  
							then _hyperscript.processNode(#stepsForm) 
							" data-keyprefix="[0][params]">
							<svg class="w-2 h-3">
								<use href="/assets/svgs/fa-sprites/solid.svg#xmark"></use>
							</svg>
						</a>
						<a class="text-red-700 cursor-pointer" _="on click remove the closest parent <div.paramRow/>">
							<svg class="w-2 h-3">
								<use href="/assets/svgs/fa-sprites/solid.svg#xmark"></use>
							</svg>
						</a>
					</div>
				</div>
			</div>
			<input type="radio" name="_httpOptions-0" role="tab" class="tab" aria-label="Headers" >
			<div role="tabpanel" class="tab-content px-2 py-4 space-y-2" id="[0][headers]">
				<div class="flex flex-row items-center gap-2 paramRow">
					<span class="shrink hidden assertIndicator">✅</span><input class="shrink input input-xs input-bordered w-1/3" placeholder="Key" value="" data-keyprefix="[0][headers]" _="on change set :kPrefix to \`\${my @data-keyPrefix}[\${me.value}]\` then set (next <input/>)'s @name to :kPrefix" 
						><input class="flex-1 input input-xs input-bordered w-full" placeholder="Value" name="[0][headers][]" value="" >
					<div class="shrink flex flex-row gap-1 items-center">
						<a _="on click set :nextIndex to (the closest parent <div.paramRows/>).childNodes.length 
							then set :paramTmpl to 
							#paramRowTmpl.innerHTML.replaceAll(#stepsForm's @data-defaultKeyPrefix, @data-keyPrefix).replaceAll('[aidx]', \`[\${:nextIndex}]\`)
							then put :paramTmpl after the closest parent <div.paramRow/>  
							then _hyperscript.processNode(#stepsForm) 
							" data-keyprefix="[0][headers]">
							<svg class="w-2 h-3">
								<use href="/assets/svgs/fa-sprites/solid.svg#plus"></use>
							</svg>
						</a>
						<a class="text-red-700 cursor-pointer" _="on click remove the closest parent <div.paramRow/>">
							<svg class="w-2 h-3">
								<use href="/assets/svgs/fa-sprites/solid.svg#xmark"></use>
							</svg>
						</a>
					</div>
				</div>
			</div>
			<input type="radio" name="_httpOptions-0" role="tab" class="tab" aria-label="Body" 
				>
			<div role="tabpanel" class="tab-content px-2 py-4">
				<select class="peer select select-sm select-bordered" data-chosen="json" onchange="this.dataset.chosen = this.value;"
					>
					<option selected="selected">
						json
					</option>
					<option>raw</option>
				</select>
				<div class="hidden peer-data-[chosen=json]:block">
					<textarea class="w-full" name="[0][json]"
						>null</textarea>
				</div>
				<div class="hidden peer-data-[chosen=raw]:block">
					<textarea class="w-full" name="[0][raw]"
						></textarea>
				</div>
			</div>
		</div>
		<div class="">
			<h5 class="label-text p-1 mb-2">Assertions</h5>
			<div class="text-sm space-y-2 px-2 [&amp;_.assertIndicator]:inline-block paramRows" id="[0][asserts]">
				<div class="flex flex-row items-center gap-2 paramRow">
					<span class="shrink hidden assertIndicator">✅</span><input class="shrink input input-xs input-bordered w-1/3" placeholder="Key" value="" data-keyprefix="[0][asserts][0]" _="on change set :kPrefix to \`\${my @data-keyPrefix}[\${me.value}]\` then set (next <input/>)'s @name to :kPrefix" 
						><input class="flex-1 input input-xs input-bordered w-full" placeholder="Value" name="[0][asserts][]" value=""
						>
					<div class="shrink flex flex-row gap-1 items-center">
						<a _="on click set :nextIndex to (the closest parent <div.paramRows/>).childNodes.length 
							then set :paramTmpl to 
							#paramRowTmplAssert.innerHTML.replaceAll(#stepsForm's @data-defaultKeyPrefix, @data-keyPrefix).replaceAll('[aidx]', \`[\${:nextIndex}]\`)
							then put :paramTmpl after the closest parent <div.paramRow/>  
							then _hyperscript.processNode(#stepsForm) 
							" data-keyprefix="[0][asserts]">
							<svg class="w-2 h-3">
								<use href="/assets/svgs/fa-sprites/solid.svg#plus"></use>
							</svg>
						</a>
						<a class="text-red-700 cursor-pointer" _="on click remove the closest parent <div.paramRow/>">
							<svg class="w-2 h-3">
								<use href="/assets/svgs/fa-sprites/solid.svg#xmark"></use>
							</svg>
						</a>
					</div>
				</div>
			</div>
		</div>
		<div class="">
			<h5 class="label-text p-1 mb-2">Exports</h5>
			<div class="text-sm space-y-2 px-2 paramRows" id="[0][exports]">
				<div class="flex flex-row items-center gap-2 paramRow">
					<span class="shrink hidden assertIndicator">✅</span><input class="shrink input input-xs input-bordered w-1/3" placeholder="Key" value="" data-keyprefix="[0][exports]" 
						_="on change set :kPrefix to \`\${my @data-keyPrefix}[\${me.value}]\` then set (next <input/>)'s @name to :kPrefix" 
						><input class="flex-1 input input-xs input-bordered w-full" placeholder="Value" name="[0][exports][]" value="" 
						>
					<div class="shrink flex flex-row gap-1 items-center">
						<a _="on click set :nextIndex to (the closest parent <div.paramRows/>).childNodes.length 
							then set :paramTmpl to 
							#paramRowTmpl.innerHTML.replaceAll(#stepsForm's @data-defaultKeyPrefix, @data-keyPrefix).replaceAll('[aidx]', \`[\${:nextIndex}]\`)
							then put :paramTmpl after the closest parent <div.paramRow/>  
							then _hyperscript.processNode(#stepsForm) 
							" data-keyprefix="[0][exports]">
							<svg class="w-2 h-3">
								<use href="/assets/svgs/fa-sprites/solid.svg#plus"></use>
							</svg>
						</a>
						<a class="text-red-700 cursor-pointer" _="on click remove the closest parent <div.paramRow/>">
							<svg class="w-2 h-3">
								<use href="/assets/svgs/fa-sprites/solid.svg#xmark"></use>
							</svg>
						</a>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
    `);
  }
}
customElements.define("steps-editor", StepsEditor);
