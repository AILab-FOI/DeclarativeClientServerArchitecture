import {
  afterNextRender,
  Component,
  inject,
  input,
  model,
  output,
  signal,
  Type,
  viewChild,
  ViewContainerRef,
} from '@angular/core';
import { Dialog, DIALOG_DATA, DialogRef } from '@angular/cdk/dialog';

export function openModal<T>(
  dialog: Dialog,
  data: ModalData<T>,
): DialogRef<unknown> {
  const ref = dialog.open(ModalComponent, {
    width: '',
    height: '',
    hasBackdrop: true,
    data: data,
  });
  return ref;
}

export type ModalData<T> = {
  data: T;
  tool: { view: Type<unknown> };
  title: string;
  inputs: { [key: string]: any };
};

@Component({
  selector: 'app-modal',
  standalone: true,
  imports: [],
  templateUrl: './modal.component.html',
  styleUrl: './modal.component.scss',
})
export class ModalComponent<T> {
  readonly injectedData = inject<ModalData<T>>(DIALOG_DATA);
  public data = model<T>(this.injectedData.data);
  public tool = input(this.injectedData.tool);
  public title = input(this.injectedData.title);
  public inputs = input(this.injectedData.inputs);

  private vcr = viewChild('view', { read: ViewContainerRef });
  public query = output<T>();

  constructor() {
    afterNextRender(() => {
      if (this.vcr()) {
        let c = this.vcr().createComponent(this.tool().view);
        c.setInput('data', this.data());
        if (this.inputs()) {
          Object.keys(this.inputs()).forEach((key) => {
            c.setInput(key, this.inputs()[key]);
          });
        }
        c.instance['query'].subscribe((o: T) => {
          this.data.set(o);
        });
      }
    });
  }

  close(): void {
    this.query.emit(undefined);
  }
  save(): void {
    this.query.emit(this.data());
  }
}
